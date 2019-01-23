{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Main entry point to floskell.
module Main ( main ) where

import           Control.Applicative             ( (<|>), many, optional )
import           Control.Exception               ( catch, throw )

import           Data.Aeson
                 ( (.:?), (.=), FromJSON(..), ToJSON(..) )
import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Types                as JSON ( typeMismatch )
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Lazy               as HashMap
import           Data.List                       ( inits, sort )
import           Data.Maybe                      ( isJust )
import           Data.Monoid                     ( (<>) )

import qualified Data.Text                       as T
import           Data.Version                    ( showVersion )

import           Floskell                        ( reformat, styles )
import           Floskell.Types
                 ( Style(styleName, styleInitialState) )

import           Foreign.C.Error                 ( Errno(..), eXDEV )

import           GHC.IO.Exception                ( ioe_errno )

import           Language.Haskell.Exts
                 ( Extension(..), Language(..), classifyExtension
                 , classifyLanguage, knownExtensions, knownLanguages )

import           Options.Applicative
                 ( ParseError(..), abortOption, argument, execParser, footerDoc
                 , fullDesc, header, help, helper, hidden, info, long, metavar
                 , option, progDesc, short, str, switch )
import qualified Options.Applicative.Help.Pretty as PP

import           Paths_floskell                  ( version )

import           System.Directory
                 ( XdgDirectory(..), copyFile, copyPermissions, doesFileExist
                 , findFileWith, getAppUserDataDirectory, getCurrentDirectory
                 , getHomeDirectory, getTemporaryDirectory, getXdgDirectory
                 , removeFile, renameFile )
import           System.FilePath                 ( joinPath, splitDirectories )
import           System.IO
                 ( FilePath, hClose, hFlush, openTempFile )

-- | Program options.
data Options = Options { optStyle       :: Maybe String
                       , optLanguage    :: Maybe String
                       , optExtensions  :: [String]
                       , optConfig      :: Maybe FilePath
                       , optPrintConfig :: Bool
                       , optFiles       :: [FilePath]
                       }

-- | Program configuration.
data Config = Config { cfgStyle      :: Style
                     , cfgLanguage   :: Language
                     , cfgExtensions :: [Extension]
                     }

instance ToJSON Config where
    toJSON Config{..} = JSON.object [ "style" .= styleName cfgStyle
                                    , "language" .= show cfgLanguage
                                    , "extensions" .= map showExt cfgExtensions
                                    , "formatting" .= styleInitialState cfgStyle
                                    ]
      where
        showExt (EnableExtension x) = show x
        showExt (DisableExtension x) = "No" ++ show x
        showExt (UnknownExtension x) = x

instance FromJSON Config where
    parseJSON (JSON.Object o) = do
        style <- maybe (cfgStyle defaultConfig) lookupStyle <$> o .:? "style"
        language <- maybe (cfgLanguage defaultConfig) lookupLanguage
            <$> o .:? "language"
        extensions <- maybe (cfgExtensions defaultConfig) (map lookupExtension)
            <$> o .:? "extensions"
        let flex = styleInitialState style
        flex' <- maybe flex (updateFlexConfig flex) <$> o .:? "formatting"
        let style' = style { styleInitialState = flex' }
        return $ Config style' language extensions
      where
        updateFlexConfig cfg v =
            case JSON.fromJSON $ mergeJSON (toJSON cfg) v of
                JSON.Error e -> error e
                JSON.Success x -> x

        mergeJSON JSON.Null r = r
        mergeJSON l JSON.Null = l
        mergeJSON (JSON.Object l) (JSON.Object r) =
            JSON.Object (HashMap.unionWith mergeJSON l r)
        mergeJSON _ r = r

    parseJSON v = JSON.typeMismatch "Config" v

-- | Default program configuration.
defaultConfig :: Config
defaultConfig = Config (head styles) Haskell2010 []

-- | Main entry point.
main :: IO ()
main = do
    opts <- execParser parser
    mconfig <- case optConfig opts of
        Just c -> return $ Just c
        Nothing ->
            if isJust (optStyle opts) then return Nothing else findConfig
    baseConfig <- case mconfig of
        Just path -> readConfig path
        Nothing -> return defaultConfig
    let config = mergeConfig baseConfig opts
    if optPrintConfig opts
        then BL.putStr $ JSON.encode config
        else run config (optFiles opts)
  where
    parser = info (helper <*> versioner <*> options)
                  (fullDesc
                   <> progDesc "Floskell reformats one or more Haskell modules."
                   <> header "floskell - A Haskell Source Code Pretty Printer"
                   <> footerDoc (Just (footerStyles PP.<$$> footerLanguages
                                       PP.<$$> footerExtensions)))

    versioner = abortOption (InfoMsg $ "floskell " ++ showVersion version)
                            (long "version"
                             <> help "Display program version number" <> hidden)

    options = Options
        <$> optional (option str
                             (long "style" <> short 's' <> metavar "STYLE"
                              <> help "Formatting style"))
        <*> optional (option str
                             (long "language" <> short 'L' <> metavar "LANGUAGE"
                              <> help "Select base language"))
        <*> many (option str
                         (long "extension" <> short 'X' <> metavar "EXTENSION"
                          <> help "Enable or disable language extensions"))
        <*> optional (option str
                             (long "config" <> short 'c' <> metavar "FILE"
                              <> help "Configuration file"))
        <*> switch (long "print-config" <> help "Print configuration")
        <*> many (argument str
                           (metavar "FILES"
                            <> help "Input files (will be replaced)"))

    footerStyles =
        makeFooter "Supported styles:" (map (T.unpack . styleName) styles)

    footerLanguages =
        makeFooter "Supported languages:" (map show knownLanguages)

    footerExtensions =
        makeFooter "Supported extensions:"
                   [ show e | EnableExtension e <- knownExtensions ]

    makeFooter hdr xs =
        PP.empty PP.<$$> PP.text hdr PP.<$$> (PP.indent 2 . PP.fillSep
                                              . PP.punctuate PP.comma
                                              . map PP.text $ sort xs)

-- | Reformat files or stdin based on provided configuration.
run :: Config -> [FilePath] -> IO ()
run Config{..} files = case files of
    [] -> reformatStdin cfgStyle cfgLanguage cfgExtensions
    _ -> mapM_ (reformatFile cfgStyle cfgLanguage cfgExtensions) files

-- | Reformat stdin according to Style, Language, and Extensions.
reformatStdin :: Style -> Language -> [Extension] -> IO ()
reformatStdin style language extensions = BL.interact $
    reformatByteString style language extensions Nothing . BL.toStrict

-- | Reformat a file according to Style, Language, and Extensions.
reformatFile :: Style -> Language -> [Extension] -> FilePath -> IO ()
reformatFile style language extensions file = do
    text <- BS.readFile file
    tmpDir <- getTemporaryDirectory
    (fp, h) <- openTempFile tmpDir "floskell.hs"
    BL.hPutStr h $
        reformatByteString style language extensions (Just file) text
    hFlush h
    hClose h
    let exdev e = if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                  then copyFile fp file >> removeFile fp
                  else throw e
    copyPermissions file fp
    renameFile fp file `catch` exdev

-- | Reformat a ByteString according to Style, Language, and Extensions.
reformatByteString :: Style
                   -> Language
                   -> [Extension]
                   -> Maybe FilePath
                   -> BS.ByteString
                   -> BL.ByteString
reformatByteString style language extensions mpath text =
    either error id $ reformat style language extensions mpath text

-- | Try to find a configuration file based on current working
-- directory, or in one of the application configuration directories.
findConfig :: IO (Maybe FilePath)
findConfig = do
    dotfilePaths <- sequence [ getHomeDirectory, getXdgDirectory XdgConfig "" ]
    dotfileConfig <- findFileWith doesFileExist dotfilePaths ".floskell.json"
    userPaths <- sequence [ getAppUserDataDirectory "floskell"
                          , getXdgDirectory XdgConfig "floskell"
                          ]
    userConfig <- findFileWith doesFileExist userPaths "config.json"
    localPaths <- map joinPath . reverse . drop 1 . inits . splitDirectories
        <$> getCurrentDirectory
    localConfig <- findFileWith doesFileExist localPaths "floskell.json"
    return $ localConfig <|> userConfig <|> dotfileConfig

-- | Load a configuration file.
readConfig :: FilePath -> IO Config
readConfig file = do
    text <- BS.readFile file
    either (error . (++) (file ++ ": ")) return $ JSON.eitherDecodeStrict text

-- | Update the program configuration from the program options.
mergeConfig :: Config -> Options -> Config
mergeConfig cfg@Config{..} Options{..} =
    cfg { cfgStyle      = maybe cfgStyle lookupStyle optStyle
        , cfgLanguage   = maybe cfgLanguage lookupLanguage optLanguage
        , cfgExtensions = cfgExtensions ++ map lookupExtension optExtensions
        }

-- | Lookup a style by name.
lookupStyle :: String -> Style
lookupStyle name = case filter ((== T.pack name) . styleName) styles of
    [] -> error $ "Unknown style: " ++ name
    x : _ -> x

-- | Lookup a language by name.
lookupLanguage :: String -> Language
lookupLanguage name = case classifyLanguage name of
    UnknownLanguage _ -> error $ "Unknown language: " ++ name
    x -> x

-- | Lookup an extension by name.
lookupExtension :: String -> Extension
lookupExtension name = case classifyExtension name of
    UnknownExtension _ -> error $ "Unkown extension: " ++ name
    x -> x

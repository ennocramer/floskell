{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Main entry point to floskell.
module Main ( main ) where

import           Control.Applicative             ( many, optional )
import           Control.Exception               ( catch, throw )

import qualified Data.Aeson.Encode.Pretty        as JSON ( encodePretty )
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BL
import           Data.List                       ( sort )
import           Data.Maybe                      ( isJust )
import           Data.Monoid                     ( (<>) )

import qualified Data.Text                       as T
import           Data.Version                    ( showVersion )

import           Floskell
                 ( AppConfig(..), defaultAppConfig, findAppConfig, readAppConfig
                 , reformat, setExtensions, setLanguage, setStyle, styles )
import           Floskell.Types                  ( Style(styleName) )

import           Foreign.C.Error                 ( Errno(..), eXDEV )

import           GHC.IO.Exception                ( ioe_errno )

import           Language.Haskell.Exts
                 ( Extension(..), Language(..), knownExtensions, knownLanguages )

import           Options.Applicative
                 ( ParseError(..), abortOption, argument, execParser, footerDoc
                 , fullDesc, header, help, helper, hidden, info, long, metavar
                 , option, progDesc, short, str, switch )
import qualified Options.Applicative.Help.Pretty as PP

import           Paths_floskell                  ( version )

import           System.Directory
                 ( copyFile, copyPermissions, getTemporaryDirectory, removeFile
                 , renameFile )
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

-- | Main entry point.
main :: IO ()
main = do
    opts <- execParser parser
    mconfig <- case optConfig opts of
        Just c -> return $ Just c
        Nothing ->
            if isJust (optStyle opts) then return Nothing else findAppConfig
    baseConfig <- case mconfig of
        Just path -> readAppConfig path
        Nothing -> return defaultAppConfig
    let config = mergeAppConfig baseConfig opts
    if optPrintConfig opts
        then BL.putStr $ JSON.encodePretty config
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
run :: AppConfig -> [FilePath] -> IO ()
run AppConfig{..} files = case files of
    [] -> reformatStdin appStyle appLanguage appExtensions
    _ -> mapM_ (reformatFile appStyle appLanguage appExtensions) files

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

-- | Update the program configuration from the program options.
mergeAppConfig :: AppConfig -> Options -> AppConfig
mergeAppConfig cfg Options{..} = cfg `setStyle` optStyle
    `setLanguage` optLanguage `setExtensions` optExtensions

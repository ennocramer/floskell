{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Main entry point to floskell.
module Main ( main ) where

import           Control.Applicative             ( (<|>), many, optional )
import           Control.Exception               ( catch, throw )

import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as BL
import           Data.Maybe                      ( fromMaybe )
import           Data.Monoid                     ( (<>) )

import qualified Data.Text                       as T
import           Data.Version                    ( showVersion )

import           Floskell                        ( reformat, styles )
import           Floskell.Types                  ( Style(styleName, styleDefConfig)
                                                 , configMaxColumns )

import           Foreign.C.Error                 ( Errno(..), eXDEV )

import           GHC.IO.Exception                ( ioe_errno )

import           Language.Haskell.Exts           ( Extension(..), Language(..)
                                                 , classifyExtension
                                                 , classifyLanguage
                                                 , knownExtensions
                                                 , knownLanguages )

import           Options.Applicative             ( ParseError(..), abortOption
                                                 , argument, auto, execParser
                                                 , footerDoc, fullDesc, header
                                                 , help, helper, hidden, info
                                                 , long, metavar, option
                                                 , progDesc, short, str, switch )
import qualified Options.Applicative.Help.Pretty as PP

import           Paths_floskell                  ( version )

import           System.Directory                ( copyFile, copyPermissions
                                                 , getTemporaryDirectory
                                                 , removeFile, renameFile )
import           System.IO                       ( hClose, hFlush, openTempFile )

-- | Program options.
data Options = Options { optStyle      :: Maybe String
                       , optLineLength :: Maybe Int
                       , optLanguage   :: Maybe String
                       , optExtensions :: [String]
                       , optFiles      :: [FilePath]
                       }

-- | Main entry point.
main :: IO ()
main = run =<< execParser parser
  where
    parser =
        info (helper <*> versioner <*> options)
             (fullDesc
                  <> progDesc "Floskell reformats one or more Haskell modules."
                  <> header "floskell - A Haskell Source Code Pretty Printer"
                  <> footerDoc (Just (footerStyles PP.<$$>
                                          footerLanguages PP.<$$>
                                          footerExtensions)))
    versioner = abortOption (InfoMsg $ "floskell " ++ showVersion version)
                            (long "version"
                                 <> help "Display program version number"
                                 <> hidden)
    options =
        Options <$> optional (option str
                                     (long "style"
                                          <> short 's'
                                          <> metavar "STYLE"
                                          <> help "Formatting style"))
                <*> optional (option auto
                                     (long "line-length"
                                          <> metavar "INT"
                                          <> help "Override style's default line length"))
                <*> optional (option str
                                     (long "language"
                                          <> short 'L'
                                          <> metavar "LANGUAGE"
                                          <> help "Select base language"))
                <*> many (option str
                                 (long "extension"
                                      <> short 'X'
                                      <> metavar "EXTENSION"
                                      <> help "Enable or disable language extensions"))
                <*> many (argument str
                                   (metavar "FILES"
                                        <> help "Input files (will be replaced)"))
    footerStyles = makeFooter "Supported styles:"
                              (map (T.unpack . styleName) styles)
    footerLanguages = makeFooter "Supported languages:"
                                 (map show knownLanguages)
    footerExtensions = makeFooter "Supported extensions:"
                                  [ show e
                                  | EnableExtension e <- knownExtensions ]
    makeFooter hdr xs =
        PP.empty
        PP.<$$>
        PP.text hdr
        PP.<$$>
        (PP.indent 2 . PP.fillSep . PP.punctuate PP.comma $ map PP.text xs)

-- | Reformat files or stdin based on provided options.
run :: Options -> IO ()
run Options{..} = case optFiles of
    [] -> reformatStdin style language extensions
    files -> mapM_ (reformatFile style language extensions) files
  where
    style = setLineLength optLineLength .
        fromMaybe (head styles) $ fmap lookupStyle optStyle
    language = fromMaybe Haskell2010 $ fmap lookupLanguage optLanguage
    extensions = fmap lookupExtension optExtensions

    setLineLength Nothing s = s
    setLineLength (Just l) s =
        s { styleDefConfig = (styleDefConfig s) { configMaxColumns = fromIntegral l
                                                }
          }

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

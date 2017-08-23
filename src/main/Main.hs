{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Main entry point to floskell.
module Main where

import           Control.Applicative

import           Control.Exception

import qualified Data.ByteString            as S
import qualified Data.ByteString.Builder    as S
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import           Data.Text                  ( Text )
import qualified Data.Text                  as T
import           Data.Version               ( showVersion )

import           Descriptive
import           Descriptive.Options

import           Floskell
import           Floskell.Types

import           Foreign.C.Error

import           GHC.IO.Exception

import           Language.Haskell.Exts      hiding ( Style, style )

import           Paths_floskell             ( version )

import           System.Directory
import           System.Environment
import           System.IO

import           Text.Read

-- | Main entry point.
main :: IO ()
main = do
    args <- getArgs
    case consume options (map T.pack args) of
        Succeeded (style, exts, mfilepath) -> case mfilepath of
            Just filepath -> do
                text <- S.readFile filepath
                tmpDir <- getTemporaryDirectory
                (fp, h) <- openTempFile tmpDir "floskell.hs"
                L8.hPutStr h
                           (either error
                                   S.toLazyByteString
                                   (reformat style (Just exts) text))
                hFlush h
                hClose h
                let exdev e = if ioe_errno e == Just ((\(Errno a) -> a) eXDEV)
                              then copyFile fp filepath >> removeFile fp
                              else throw e
                copyPermissions filepath fp
                renameFile fp filepath `catch` exdev
            Nothing -> L8.interact (either error S.toLazyByteString .
                                        reformat style (Just exts) .
                                            L8.toStrict)
        Failed (Wrap (Stopped Version) _) -> putStrLn ("floskell " ++
                                                           showVersion version)
        _ -> error (T.unpack (textDescription (describe options [])))

-- | Options that stop the argument parser.
data Stoppers = Version
    deriving (Show)

-- | Program options.
options :: Monad m
        => Consumer [Text] (Option Stoppers) m ( Style
                                               , [Extension]
                                               , Maybe FilePath
                                               )
options = ver *>
    ((,,) <$> style <*> exts <*> file)
  where
    ver = stop (flag "version" "Print the version" Version)
    style = makeStyle <$> (constant "--style" "Style to print with" () *>
                               foldr1 (<|>)
                                      (map (\s -> constant (styleName s)
                                                           (styleDescription s)
                                                           s)
                                           styles))
                      <*> lineLen
    exts = fmap getExtensions (many (prefix "X" "Language extension"))
    lineLen = fmap (>>= (readMaybe . T.unpack))
                   (optional (arg "line-length" "Desired length of lines"))
    makeStyle s mlen = case mlen of
        Nothing -> s
        Just len ->
            s { styleDefConfig = (styleDefConfig s) { configMaxColumns = len }
              }
    file = fmap (fmap T.unpack) (optional (anyString "[<filename>]"))

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint
-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where
    f _ "Haskell98" = []
    f a ('N' : 'o' : x) | Just x' <- readExtension x = delete x' a
    f a x | Just x' <- readExtension x = x' :
                delete x' a
    f _ x = error $ "Unknown extension: " ++ x

-- | Parse an extension.
readExtension :: String -> Maybe Extension
readExtension x = case classifyExtension x of
    UnknownExtension _ -> Nothing
    x' -> Just x'

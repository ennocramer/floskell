{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Control.Monad
import Data.List (find, unfoldr, isPrefixOf)

import System.Directory
import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import qualified HIndent

testDir, expectedDir :: String
testDir = "tests"
expectedDir = "expected"

-- This executable generates a test suite for a style using a test suite
-- from a previous style. Given arguments `from` and `to` which are style names,
-- it will take all the input files for `from`, create identical input files for `to`,
-- run each block in the input files through the `to` style, and generate expectation files matching the output.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [from, to] -> generateTests from to
    _          -> putStrLn "Two arguments required: source tests and destination tests."

generateTests :: String -> String -> IO ()
generateTests from to = do
  --  Find the source and destination styles.
  let Just toStyle = findStyle (T.pack to)

  --  Get all the test and expectation files for the original test suite.
  testFilenamesFrom <- filter (not . isPrefixOf ".") <$> getDirectoryContents (tests from)

  -- Verify the target directories exist.
  createDirectoryIfMissing True $ tests to
  createDirectoryIfMissing True $ expected to

  -- Copy test files to the new style test directory.
  forM_ testFilenamesFrom $ \filename ->
    copyFile (tests from ++ filename) (tests to ++ filename)

  -- Generate expectation files for the new style test directory.
  forM_ testFilenamesFrom $ \filename -> do
    let dstFilename = expected to ++ expectedFilename filename
    contents <- expectationFileContents toStyle <$> S.readFile (tests to ++ filename)
    S.writeFile dstFilename contents

  where
    findStyle style = find ((== style). HIndent.styleName) HIndent.styles
    tests style = "test/" ++ style ++ "/" ++ testDir ++ "/"
    expected style = "test/" ++ style ++ "/" ++ expectedDir ++ "/"
    expectedFilename filename = take (length filename - 4) filename ++ ("exp" :: String)

expectationFileContents :: HIndent.Style -> ByteString -> ByteString
expectationFileContents style contents =
  let testDecls = parsePieces contents
      fmt input = L.toStrict . S.toLazyByteString $ case HIndent.reformat style Nothing input of
                      Left err      -> error err
                      Right builder -> builder
      outputs = map (replaceEmptyNewlines . fmt) testDecls
  in S.intercalate "\n" outputs
  where
    replaceEmptyNewlines = S.unlines . map replaceNewline . S.lines
    replaceNewline "" = ">"
    replaceNewline x = x

parsePieces :: ByteString -> [ByteString]
parsePieces str = map (S.intercalate "\n" . map mkNewlines) pieces
  where
    pieces = unfoldr unfolder (S.lines str)

    unfolder :: [ByteString] -> Maybe ([ByteString], [ByteString])
    unfolder [] = Nothing
    unfolder remaining = Just $
     case break pieceBreak (zip remaining (tail remaining ++ [""]))  of
       (nonNull, [])     -> (map fst nonNull, [])
       (nonNull, _:rest) -> (map fst nonNull, map fst rest)

    pieceBreak :: (ByteString, ByteString) -> Bool
    pieceBreak ("", "") = error "Two consecutive line breaks!"
    pieceBreak (line, next) = S.null line && S.head next /= ' '

    mkNewlines :: ByteString -> ByteString
    mkNewlines ">" = ""
    mkNewlines x = x

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad
import           Control.Applicative
import           Data.List (find, unfoldr, break, isPrefixOf, intercalate)

import           Test.Hspec
import           System.Directory
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Language.Haskell.Exts.Extension

import qualified HIndent

styles :: [FilePath]
styles = ["fundamental", "gibiansky", "chris-done", "johan-tibell", "cramer"]

testDir, expectedDir :: String
testDir = "tests"
expectedDir = "expected"

main :: IO ()
main = forM_ styles testStyle

testStyle :: FilePath -> IO ()
testStyle style = do
  let Just theStyle = find ((== T.pack style) . HIndent.styleName) HIndent.styles
  testFilenames <- filter (not . isPrefixOf ".") <$> getDirectoryContents tests
  let expFiles = map ((expected ++) . expectedFilename) testFilenames
      testFiles = map (tests ++) testFilenames
  specs <- forM (zip testFiles expFiles) (uncurry $ useTestFiles theStyle)
  hspec $ foldl1 (>>) specs

  where
    tests = "test/" ++ style ++ "/" ++ testDir ++ "/"
    expected = "test/" ++ style ++ "/" ++ expectedDir ++ "/"

    expectedFilename filename = take (length filename - 4) filename ++ ("exp" :: String)

useTestFiles :: HIndent.Style -> FilePath -> FilePath -> IO Spec
useTestFiles style test exp = do
  testContents <- S.readFile test
  expContents <- S.readFile exp
  let testDecls = parsePieces testContents
      expDecls = parsePieces expContents
  when (length testDecls /= length expDecls) $
    error $ concat
              [ "Mismatched number of pieces in files "
              , test
              , " ("
              , show $ length testDecls
              , ")"
              , " and "
              , exp
              , " ("
              , show $ length expDecls
              , ")"
              ]
  return $ describe ("hindent applied to chunks in " ++ test) $ foldl1 (>>) $ zipWith (mkSpec style)
                                                                                testDecls expDecls

mkSpec :: HIndent.Style -> S.ByteString -> S.ByteString -> Spec
mkSpec style input desired = it "works" $
  case HIndent.reformat style (Just exts) input of
    Left err      -> expectationFailure ("Error: " ++ err)
    Right builder -> L.toStrict (S.toLazyByteString builder) `shouldBe` desired
  where exts =
          glasgowExts ++
          map EnableExtension [TemplateHaskell,DataKinds, MultiWayIf]

parsePieces :: S.ByteString -> [S.ByteString]
parsePieces str = map (S.intercalate "\n" . map mkNewlines) pieces
  where
    pieces = unfoldr unfolder (S.lines str)

    unfolder :: [S.ByteString] -> Maybe ([S.ByteString], [S.ByteString])
    unfolder [] = Nothing
    unfolder remaining = Just $
      case break pieceBreak (zip remaining (tail remaining ++ [""])) of
        (nonNull, [])     -> (map fst nonNull, [])
        (nonNull, _:rest) -> (map fst nonNull, map fst rest)

    pieceBreak :: (S.ByteString, S.ByteString) -> Bool
    pieceBreak ("", "") = error $ "Two consecutive line breaks in:\n" ++ S.unpack str
    pieceBreak (line, next) = S.null line && S.head next /= ' '

    mkNewlines :: S.ByteString -> S.ByteString
    mkNewlines ">" = ""
    mkNewlines x = x

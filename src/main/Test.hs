{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test the pretty printer.
module Main where

import           Control.Applicative    ( (<|>) )
import           Control.Monad          ( forM_, guard )

import           Data.Maybe             ( mapMaybe )
import qualified Data.Text              as T
import           Data.Text.Lazy         ( Text )
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB ( toLazyText )
import qualified Data.Text.Lazy.IO      as TIO

import           Floskell

import           Language.Haskell.Exts  ( Language(Haskell2010) )

import           Markdone               ( Markdone(..) )
import qualified Markdone               as MD

import           System.Environment     ( getArgs )

import           Test.Hspec

data TestTree =
    TestSection String [TestTree] | TestSnippet Text | TestMismatchMarker

-- | Prints a string without quoting and escaping.
newtype Readable = Readable Text
    deriving ( Eq )

instance Show Readable where
    show (Readable x) = "\n" ++ TL.unpack x

-- | Version of 'shouldBe' that prints strings in a readable way,
-- better for our use-case.
shouldBeReadable :: Text -> Text -> Expectation
shouldBeReadable x y = Readable x `shouldBe` Readable y

haskell :: Text
haskell = "haskell"

referenceFile :: Style -> String
referenceFile style = "styles/" ++ name ++ ".md"
  where
    name = T.unpack $ styleName style

loadMarkdone :: String -> IO [Markdone]
loadMarkdone filename = do
    bytes <- TIO.readFile filename
    MD.parse (MD.tokenize bytes)

saveMarkdone :: String -> [Markdone] -> IO ()
saveMarkdone filename doc =
    TIO.writeFile filename . TB.toLazyText $ MD.print doc

-- | Extract code snippets from a Markdone document.
extractSnippets :: Text -> [Markdone] -> [TestTree]
extractSnippets lang = mapMaybe convert
  where
    convert (Section name children) = return $ TestSection (TL.unpack name) $
        extractSnippets lang children
    convert (CodeFence l c) = do
        guard $ l == lang
        return $ TestSnippet c
    convert _ = Nothing

-- | Load haskell code snippets from Markdone document.
loadSnippets :: String -> IO [TestTree]
loadSnippets filename = do
    doc <- loadMarkdone filename
    return $ extractSnippets haskell doc

disabled :: T.Text -> [Int] -> Maybe String
disabled style path = lookup (Just style, path) disabledTests
    <|> lookup (Nothing :: Maybe T.Text, path) disabledTests
  where
    disabledTests =
        []
#if MIN_VERSION_haskell_src_exts(1,21,0)
#else
        ++ [ ((Nothing, [ 2, 3, 4, 1 ]), "requires haskell-src-exts >=1.21.0")
           ]
#if MIN_VERSION_haskell_src_exts(1,20,0)
#else
        ++ [ ((Nothing, [ 2, 3, 6, 1 ]), "requires haskell-src-exts >=1.20.0")
           , ((Nothing, [ 2, 3, 11, 1 ]), "requires haskell-src-exts >=1.20.0")
           , ((Nothing, [ 2, 3, 12, 1 ]), "requires haskell-src-exts >=1.20.0")
           , ((Nothing, [ 2, 4, 1, 1 ]), "requires haskell-src-exts >=1.20.0")
           , ((Nothing, [ 2, 4, 9, 1 ]), "requires haskell-src-exts >=1.20.0")
           ]
#endif
#endif

-- | Convert the Markdone document to Spec benchmarks.
toSpec :: Style -> [Int] -> [TestTree] -> [TestTree] -> Spec
toSpec style path inp ref =
    forM_ (zip3 [ 1 :: Int .. ] inp (ref ++ repeat TestMismatchMarker)) $ \case
        (n, TestSection title children, TestSection _ children') ->
            describe (title ++ show (path ++ [ n ])) $
            toSpec style (path ++ [ n ]) children children'
        (n, TestSnippet code, TestSnippet code') ->
            case disabled (styleName style) (path ++ [ n ]) of
                Just msg -> it "Disabled" $ pendingWith msg
                Nothing -> do
                    it (name n "formats as expected") $
                        case reformatSnippet style code of
                            Left e -> error e
                            Right b -> b `shouldBeReadable` code'
                    it (name n "formatting is idempotent") $
                        case reformatSnippet style code of
                            Left e -> error e
                            Right b -> case reformatSnippet style b of
                                Left e -> error e
                                Right b' -> b' `shouldBeReadable` b
        (n, _, _) -> error $ name n "structure mismatch in reference file"
  where
    name n desc = "Snippet " ++ show n ++ " - " ++ desc

-- | Main tests.
testAll :: IO ()
testAll = do
    input <- loadSnippets "TEST.md"
    refs <- mapM loadRef styles
    hspec $ forM_ refs $
        \(name, style, ref) -> context name $ toSpec style [] input ref
  where
    loadRef style = do
        let name = T.unpack $ styleName style
        tree <- loadSnippets $ referenceFile style
        return (name, style, tree)

reformatSnippet :: Style -> Text -> Either String Text
reformatSnippet style =
    reformat (AppConfig style Haskell2010 defaultExtensions [])
             (Just "TEST.md")

regenerate :: Style -> [Markdone] -> [Markdone]
regenerate style = map fmt
  where
    fmt (CodeFence lang code) =
        if lang == haskell
        then CodeFence lang $ either (TL.pack . ("-- " ++) . show) id $
            reformatSnippet style code
        else CodeFence lang code
    fmt (Section heading children) =
        Section heading $ regenerate style children
    fmt x = x

-- | Regenerate style reference files.
regenerateAll :: IO ()
regenerateAll = do
    doc <- loadMarkdone "TEST.md"
    forM_ styles $ \style -> saveMarkdone (referenceFile style) $
        regenerate style doc

main :: IO ()
main = do
    args <- getArgs
    case args of
        [ "regenerate" ] -> regenerateAll
        _ -> testAll

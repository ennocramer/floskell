{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Haskell indenter.
module Floskell
    ( -- * Formatting functions.
      reformat
    , prettyPrint
      -- * Style
    , Style(..)
    , styles
      -- * Testing
    , defaultExtensions
    ) where

import           Data.ByteString            ( ByteString )
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Internal   as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8       as UTF8
import qualified Data.ByteString.Unsafe     as S
import           Data.Function              ( on )
import           Data.List
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Monoid

import qualified Floskell.Buffer            as Buffer
import           Floskell.Comments
import           Floskell.Pretty            ( pretty, printComment )
import           Floskell.Styles            ( styles )
import           Floskell.Types

import           Language.Haskell.Exts
                 hiding ( Pretty, Style, parse, prettyPrint, style )
import qualified Language.Haskell.Exts      as Exts

data CodeBlock = HaskellSource Int ByteString | CPPDirectives ByteString
    deriving ( Show, Eq )

-- | Format the given source.
reformat :: Style
         -> Language
         -> [Extension]
         -> Maybe FilePath
         -> ByteString
         -> Either String L.ByteString
reformat style language langextensions mfilepath x = preserveTrailingNewline x
    . mconcat . intersperse "\n" <$> mapM processBlock (cppSplitBlocks x)
  where
    processBlock :: CodeBlock -> Either String L.ByteString
    processBlock (CPPDirectives text) = Right $ L.fromStrict text
    processBlock (HaskellSource offset text) =
        let ls = S8.lines text
            prefix = findPrefix ls
            code = unlines' (map (stripPrefix prefix) ls)
            exts = readExtensions (UTF8.toString code)
            mode'' = case exts of
                Nothing -> mode'
                Just (Nothing, exts') ->
                    mode' { extensions = exts' ++ extensions mode' }
                Just (Just lang, exts') ->
                    mode' { baseLanguage = lang
                          , extensions   = exts' ++ extensions mode'
                          }
        in
            case parseModuleWithComments mode'' (UTF8.toString code) of
                ParseOk (m, comments) ->
                    fmap (addPrefix prefix) (prettyPrint style m comments)
                ParseFailed loc e -> Left $
                    Exts.prettyPrint (loc { srcLine = srcLine loc + offset })
                    ++ ": " ++ e

    unlines' = S.concat . intersperse "\n"

    unlines'' = L.concat . intersperse "\n"

    addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
    addPrefix prefix = unlines'' . map (L8.fromStrict prefix <>) . L8.lines

    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix line = if S.null (S8.dropWhile (== '\n') line)
                              then line
                              else fromMaybe (error "Missing expected prefix")
                                  . s8_stripPrefix prefix $ line

    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines

    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))

    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt = case S8.uncons txt of
        Nothing -> ""
        Just ('>', txt') ->
            if not bracketUsed then S8.cons '>' (takePrefix True txt') else ""
        Just (c, txt') -> if c == ' ' || c == '\t'
                          then S8.cons c (takePrefix bracketUsed txt')
                          else ""

    findSmallestPrefix :: [ByteString] -> ByteString
    findSmallestPrefix [] = ""
    findSmallestPrefix ("" : _) = ""
    findSmallestPrefix (p : ps) =
        let first = S8.head p
            startsWithChar c x = S8.length x > 0 && S8.head x == c
        in
            if all (startsWithChar first) ps
            then S8.cons first (findSmallestPrefix (S.tail p : map S.tail ps))
            else ""

    mode' = defaultParseMode { parseFilename = fromMaybe "<stdin>" mfilepath
                             , baseLanguage  = language
                             , extensions    = langextensions
                             }

    preserveTrailingNewline x x' = if not (S8.null x) && S8.last x == '\n'
                                       && not (L8.null x') && L8.last x' /= '\n'
                                   then x' <> "\n"
                                   else x'

-- | Break a Haskell code string into chunks, using CPP as a delimiter.
-- Lines that start with '#if', '#end', or '#else' are their own chunks, and
-- also act as chunk separators. For example, the code
--
-- > #ifdef X
-- > x = X
-- > y = Y
-- > #else
-- > x = Y
-- > y = X
-- > #endif
--
-- will become five blocks, one for each CPP line and one for each pair of declarations.
cppSplitBlocks :: ByteString -> [CodeBlock]
cppSplitBlocks = map (classify . unlines') . groupBy ((==) `on` (cppLine . snd))
    . zip [ 0 .. ] . S8.lines
  where
    cppLine :: ByteString -> Bool
    cppLine src =
        any (`S8.isPrefixOf` src)
            [ "#if"
            , "#end"
            , "#else"
            , "#define"
            , "#undef"
            , "#elif"
            , "#include"
            , "#error"
            , "#warning"
            ]

    unlines' :: [(Int, ByteString)] -> (Int, ByteString)
    unlines' [] = (0, "")
    unlines' xs@((line, _) : _) = (line, S8.unlines $ map snd xs)

    classify :: (Int, ByteString) -> CodeBlock
    classify (ofs, text) =
        if cppLine text then CPPDirectives text else HaskellSource ofs text

-- | Print the module.
prettyPrint :: Style
            -> Module SrcSpanInfo
            -> [Comment]
            -> Either a L.ByteString
prettyPrint style m comments =
    let (cs, ast) =
            annotateComments (fromMaybe m $ applyFixities baseFixities m)
                             comments
        csComments = map comInfoComment cs
    in
        Right (runPrinterStyle style
                               -- For the time being, assume that all "free-floating" comments come at the beginning.
                               -- If they were not at the beginning, they would be after some ast node.
                               -- Thus, print them before going for the ast.
                               (do
                                    mapM_ (printComment Nothing)
                                          (reverse csComments)
                                    pretty ast))

-- | Pretty print the given printable thing.
runPrinterStyle :: Style -> Printer () -> L.ByteString
runPrinterStyle (Style _name _author _desc st) m =
    maybe (error "Printer failed with mzero call.")
          (Buffer.toLazyByteString . psBuffer)
          (snd <$> execPrinter m
                               (PrintState Buffer.empty
                                           0
                                           0
                                           Map.empty
                                           st
                                           False
                                           Anything))

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions = [ e | e@EnableExtension{} <- knownExtensions ]
    \\ map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [ Arrows -- steals proc
    , TransformListComp -- steals the group keyword
    , XmlSyntax
    , RegularPatterns -- steals a-b
    , UnboxedTuples -- breaks (#) lens operator
    , PatternSynonyms -- steals the pattern keyword
    , RecursiveDo -- steals the rec keyword
    , DoRec -- same
    , TypeApplications -- since GHC 8 and haskell-src-exts-1.19
    ]

-- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
    | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
    | otherwise = Nothing

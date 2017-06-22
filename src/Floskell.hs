{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables #-}

-- | Haskell indenter.

module Floskell
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Style
  ,Style(..)
  ,styles
  ,chrisDone
  ,johanTibell
  ,fundamental
  ,gibiansky
  ,cramer
  -- * Testing
  ,defaultExtensions
  )
  where

import           Floskell.Comments
import           Floskell.Pretty
import           Floskell.Styles.ChrisDone (chrisDone)
import           Floskell.Styles.Cramer (cramer)
import           Floskell.Styles.Fundamental (fundamental)
import           Floskell.Styles.Gibiansky (gibiansky)
import           Floskell.Styles.JohanTibell (johanTibell)
import           Floskell.Types

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as S
import           Data.Function (on)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Language.Haskell.Exts hiding (Style, prettyPrint, Pretty, style, parse)

data CodeBlock = HaskellSource ByteString
               | CPPDirectives ByteString
  deriving (Show, Eq)

-- | Format the given source.
reformat :: Style -> Maybe [Extension] -> ByteString -> Either String Builder
reformat style mexts x =
  mconcat . intersperse "\n" <$> mapM processBlock (cppSplitBlocks x)
  where
    processBlock :: CodeBlock -> Either String Builder
    processBlock (CPPDirectives text) = Right $ S.byteString text
    processBlock (HaskellSource text) =
      let ls = S8.lines text
          prefix = findPrefix ls
          code = unlines' (map (stripPrefix prefix) ls)
      in case parseModuleWithComments mode' (UTF8.toString code) of
        ParseOk (m, comments) ->
          fmap (S.lazyByteString .
          addPrefix prefix .
          S.toLazyByteString)
          (prettyPrint mode' style m comments)
        ParseFailed _ e -> Left e

    unlines' = S.concat . intersperse "\n"
    unlines'' = L.concat . intersperse "\n"

    addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
    addPrefix prefix = unlines'' . map (L8.fromStrict prefix <>) . L8.lines

    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix line =
      if S.null (S8.dropWhile (== '\n') line)
      then line
      else fromMaybe (error "Missing expected prefix") . s8_stripPrefix prefix $ line

    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines

    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))

    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt =
      case S8.uncons txt of
        Nothing -> ""
        Just ('>', txt') -> if not bracketUsed
                              then S8.cons '>' (takePrefix True txt')
                              else ""
        Just (c, txt') -> if c == ' ' || c == '\t'
                            then S8.cons c (takePrefix bracketUsed txt')
                            else ""


    findSmallestPrefix :: [ByteString] -> ByteString
    findSmallestPrefix [] = ""
    findSmallestPrefix ("":_) = ""
    findSmallestPrefix (p:ps) =
      let first = S8.head p
          startsWithChar c x  = S8.length x > 0 && S8.head x == c
      in if all (startsWithChar first) ps
           then S8.cons first (findSmallestPrefix (S.tail p : map S.tail ps))
           else ""

    mode' =
      case mexts of
        Just exts -> parseMode { extensions = exts }
        Nothing   -> parseMode

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
cppSplitBlocks inp =
  modifyLast (inBlock (<> trailing)) .
  map (classify . mconcat . intersperse "\n") .
  groupBy ((==) `on` cppLine) . S8.lines $ inp
  where
    cppLine :: ByteString -> Bool
    cppLine src =
      any
        (`S8.isPrefixOf` src)
        ["#if", "#end", "#else", "#define", "#undef", "#elif"]
    classify :: ByteString -> CodeBlock
    classify text =
      if cppLine text
        then CPPDirectives text
        else HaskellSource text
    -- Hack to work around some parser issues in haskell-src-exts: Some pragmas
    -- need to have a newline following them in order to parse properly, so we include
    -- the trailing newline in the code block if it existed.
    trailing :: ByteString
    trailing = if S8.isSuffixOf "\n" inp then "\n" else ""
    modifyLast :: (a -> a) -> [a] -> [a]
    modifyLast _ [] = []
    modifyLast f [x] = [f x]
    modifyLast f (x:xs) = x : modifyLast f xs
    inBlock :: (ByteString -> ByteString) -> CodeBlock -> CodeBlock
    inBlock f (HaskellSource txt) = HaskellSource (f txt)
    inBlock _ dir = dir

-- | Print the module.
prettyPrint :: ParseMode
            -> Style
            -> Module SrcSpanInfo
            -> [Comment]
            -> Either a Builder
prettyPrint mode' style m comments =
  let (cs,ast) =
        annotateComments (fromMaybe m $ applyFixities baseFixities m) comments
      csComments = map comInfoComment cs
  in case style of
      style@(Style { styleCommentPreprocessor = preprocessor }) ->
        Right (runPrinterStyle
                  mode'
                  style
                  -- For the time being, assume that all "free-floating" comments come at the beginning.
                  -- If they were not at the beginning, they would be after some ast node.
                  -- Thus, print them before going for the ast.
                  (do comments <- preprocessor (reverse csComments)
                      mapM_ (printComment Nothing) comments
                      pretty ast))

-- | Pretty print the given printable thing.
runPrinterStyle
  :: ParseMode -> Style -> (forall s. Printer s ()) -> Builder
runPrinterStyle mode' (Style _name _author _desc st extenders config preprocessor penalty) m =
  maybe (error "Printer failed with mzero call.")
        psOutput
        (snd <$> execPrinter
           m
           (PrintState 0 mempty False 0 1 st extenders config False False mode' preprocessor penalty Anything))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}
  where allExtensions =
          filter isDisabledExtention knownExtensions
        isDisabledExtention (DisableExtension _) = False
        isDisabledExtention _ = True

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles =
  [fundamental,chrisDone,johanTibell,gibiansky,cramer]

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions =
  [e | e@EnableExtension{} <- knownExtensions] \\
  map EnableExtension badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ]

s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
   | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
   | otherwise = Nothing

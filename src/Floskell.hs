{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Haskell indenter.
module Floskell
    ( -- * Configuration
      AppConfig(..)
    , defaultAppConfig
    , findAppConfig
    , findAppConfigIn
    , readAppConfig
    , setStyle
    , setLanguage
    , setExtensions
    , setFixities
      -- * Formatting functions.
    , reformat
      -- * Style
    , Style(..)
    , styles
      -- * Testing
    , defaultExtensions
    , knownFixities
    ) where

import           Data.ByteString.Lazy       ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as UTF8

import           Data.Function              ( on )
import           Data.List
import           Data.Maybe
import           Data.Monoid

import qualified Floskell.Buffer            as Buffer
import           Floskell.Comments
import           Floskell.Config
import           Floskell.ConfigFile
import           Floskell.Pretty            ( pretty )
import           Floskell.Styles            ( Style(..), styles )
import           Floskell.Types

import           Language.Haskell.Exts
                 hiding ( Pretty, Style, parse, prettyPrint, style )
import qualified Language.Haskell.Exts      as Exts

data CodeBlock = HaskellSource Int [ByteString] | CPPDirectives [ByteString]
    deriving ( Show, Eq )

trimBy :: (a -> Bool) -> [a] -> ([a], [a], [a])
trimBy f xs = (prefix, middle, suffix)
  where
    (prefix, xs') = span f xs

    (suffix', middle') = span f $ reverse xs'

    middle = reverse middle'

    suffix = reverse suffix'

findLinePrefix :: (Char -> Bool) -> [ByteString] -> ByteString
findLinePrefix _ [] = ""
findLinePrefix f (x : xs') = go (L8.takeWhile f x) xs'
  where
    go prefix xs = if all (prefix `L8.isPrefixOf`) xs
                   then prefix
                   else go (L8.take (L8.length prefix - 1) prefix) xs

findIndent :: (Char -> Bool) -> [ByteString] -> ByteString
findIndent _ [] = ""
findIndent f (x : xs') = go (L8.takeWhile f x) $ filter (not . L8.all f) xs'
  where
    go indent xs = if all (indent `L8.isPrefixOf`) xs
                   then indent
                   else go (L8.take (L8.length indent - 1) indent) xs

preserveVSpace :: Monad m
               => ([ByteString] -> m [ByteString])
               -> [ByteString]
               -> m [ByteString]
preserveVSpace format input = do
    output <- format input'
    return $ prefix ++ output ++ suffix
  where
    (prefix, input', suffix) = trimBy L8.null input

preservePrefix :: Monad m
               => (Int -> [ByteString] -> m [ByteString])
               -> [ByteString]
               -> m [ByteString]
preservePrefix format input = do
    output <- format (prefixLength prefix) input'
    return $ map (prefix <>) output
  where
    prefix = findLinePrefix allowed input

    input' = map (L8.drop $ L8.length prefix) input

    allowed c = c == ' ' || c == '\t' || c == '>'

    prefixLength = sum . map (\c -> if c == '\t' then 8 else 1) . L8.unpack

preserveIndent :: Monad m
               => (Int -> [ByteString] -> m [ByteString])
               -> [ByteString]
               -> m [ByteString]
preserveIndent format input = do
    output <- format (prefixLength prefix) input'
    return $ map (prefix <>) output
  where
    prefix = findIndent allowed input

    input' = map (L8.drop $ L8.length prefix) input

    allowed c = c == ' ' || c == '\t'

    prefixLength = sum . map (\c -> if c == '\t' then 8 else 1) . L8.unpack

withReducedLineLength :: Int -> Config -> Config
withReducedLineLength offset config = config { cfgPenalty = penalty }
  where
    penalty = (cfgPenalty config) { penaltyMaxLineLength =
                                        penaltyMaxLineLength (cfgPenalty config)
                                        - offset
                                  }

-- | Format the given source.
reformat :: AppConfig
         -> Maybe FilePath
         -> ByteString
         -> Either String ByteString
reformat config mfilepath input = fmap (L8.intercalate "\n")
    . preserveVSpace (preservePrefix (reformatLines mode cfg)) $
    L8.split '\n' input
  where
    mode = case readExtensions $ UTF8.toString input of
        Nothing -> mode'
        Just (Nothing, exts') ->
            mode' { extensions = exts' ++ extensions mode' }
        Just (Just lang, exts') ->
            mode' { baseLanguage = lang
                  , extensions   = exts' ++ extensions mode'
                  }

    mode' = defaultParseMode { parseFilename = fromMaybe "<stdin>" mfilepath
                             , baseLanguage  = appLanguage config
                             , extensions    = appExtensions config
                             , fixities      =
                                   Just $ appFixities config ++ knownFixities
                             }

    cfg = styleConfig $ appStyle config

reformatLines :: ParseMode
              -> Config
              -> Int
              -> [ByteString]
              -> Either String [ByteString]
reformatLines mode config indent = mapM (fmap (L8.intercalate "\n") . fmt)
    . cppSplitBlocks
  where
    config' = withReducedLineLength indent config

    fmt (CPPDirectives lines) = Right lines
    fmt (HaskellSource offset lines) =
        preserveVSpace (preserveIndent (reformatBlock mode config' offset))
                       lines

-- | Format a continuous block of code without CPP directives.
reformatBlock :: ParseMode
              -> Config
              -> Int
              -> Int
              -> [ByteString]
              -> Either String [ByteString]
reformatBlock mode config offset indent lines =
    case parseModuleWithComments mode code of
        ParseOk (m, comments) ->
            let ast = annotateWithComments m comments
            in
                case prettyPrint (pretty ast) config' of
                    Nothing -> Left "Printer failed with mzero call."
                    Just output -> Right [ output ]
        ParseFailed loc e -> Left $
            Exts.prettyPrint (loc { srcLine = srcLine loc + offset }) ++ ": "
            ++ e
  where
    code = UTF8.toString $ L8.intercalate "\n" lines

    config' = withReducedLineLength indent config

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
cppSplitBlocks :: [ByteString] -> [CodeBlock]
cppSplitBlocks = map (classify . merge) . groupBy ((==) `on` (cppLine . snd))
    . zip [ 0 .. ]
  where
    cppLine :: ByteString -> Bool
    cppLine src =
        any (`L8.isPrefixOf` src)
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

    merge :: [(Int, ByteString)] -> (Int, [ByteString])
    merge [] = (0, [])
    merge xs@((line, _) : _) = (line, map snd xs)

    classify :: (Int, [ByteString]) -> CodeBlock
    classify (ofs, lines) = if cppLine (head lines)
                            then CPPDirectives lines
                            else HaskellSource ofs lines

prettyPrint :: Printer a -> Config -> Maybe ByteString
prettyPrint printer = fmap (Buffer.toLazyByteString . psBuffer . snd)
    . execPrinter printer . initialPrintState

knownFixities :: [Fixity]
knownFixities = baseFixities

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

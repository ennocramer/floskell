{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
    ) where

import           Data.ByteString.Lazy       ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as UTF8
import           Data.List
import           Data.Maybe
#if __GLASGOW_HASKELL__ <= 802
import           Data.Monoid
#endif

import qualified Floskell.Buffer            as Buffer
import           Floskell.Comments
import           Floskell.Config
import           Floskell.ConfigFile
import           Floskell.Fixities          ( builtinFixities )
import           Floskell.Pretty            ( pretty )
import           Floskell.Styles            ( Style(..), styles )
import           Floskell.Types

import           Language.Haskell.Exts
                 hiding ( Comment, Pretty, Style, parse, prettyPrint, style )
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
reformat
    :: AppConfig -> Maybe FilePath -> ByteString -> Either String ByteString
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
                                   Just $ appFixities config ++ builtinFixities
                             }

    cfg = safeConfig . styleConfig $ appStyle config

reformatLines
    :: ParseMode -> Config -> Int -> [ByteString] -> Either String [ByteString]
reformatLines mode config indent = format . filterPreprocessorDirectives
  where
    config' = withReducedLineLength indent config

    format (code, comments) =
        preserveVSpace (preserveIndent (reformatBlock mode config' comments))
                       code

-- | Format a continuous block of code without CPP directives.
reformatBlock :: ParseMode
              -> Config
              -> [Comment]
              -> Int
              -> [ByteString]
              -> Either String [ByteString]
reformatBlock mode config cpp indent lines =
    case parseModuleWithComments mode code of
        ParseOk (m, comments') ->
            let comments = map makeComment comments'
                ast = annotateWithComments m (mergeComments comments cpp)
            in
                case prettyPrint (pretty ast) config' of
                    Nothing -> Left "Printer failed with mzero call."
                    Just output -> Right $ L8.lines output
        ParseFailed loc e -> Left $
            Exts.prettyPrint (loc { srcLine = srcLine loc }) ++ ": " ++ e
  where
    code = UTF8.toString $ L8.intercalate "\n" lines

    config' = withReducedLineLength indent config

    makeComment (Exts.Comment inline span text) =
        Comment (if inline then InlineComment else LineComment) span text

    mergeComments xs [] = xs
    mergeComments [] ys = ys
    mergeComments xs@(x : xs') ys@(y : ys') =
        if srcSpanStartLine (commentSpan x) < srcSpanStartLine (commentSpan y)
        then x : mergeComments xs' ys
        else y : mergeComments xs ys'

-- | Remove CPP directives from input source, retur
filterPreprocessorDirectives :: [ByteString] -> ([ByteString], [Comment])
filterPreprocessorDirectives lines = (code, comments)
  where
    code = map (\l -> if cppLine l then "" else l) lines

    comments = map makeComment . filter (cppLine . snd) $ zip [ 1 .. ] lines

    makeComment (n, l) =
        Comment PreprocessorDirective
                (SrcSpan "" n 1 n (fromIntegral $ L8.length l + 1))
                (L8.unpack l)

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

prettyPrint :: Printer a -> Config -> Maybe ByteString
prettyPrint printer = fmap (Buffer.toLazyByteString . psBuffer . snd)
    . execPrinter printer . initialPrintState

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

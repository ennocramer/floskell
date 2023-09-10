{-# LANGUAGE CPP #-}
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
    ) where

import           Data.List
import           Data.Maybe
#if __GLASGOW_HASKELL__ <= 802
import           Data.Monoid
#endif
import           Data.Text.Lazy        ( Text )
import qualified Data.Text.Lazy        as TL

import qualified Floskell.Buffer       as Buffer
import           Floskell.Comments
import           Floskell.Config
import           Floskell.ConfigFile
import           Floskell.Fixities     ( builtinFixities )
import           Floskell.Pretty       ( pretty )
import           Floskell.Styles       ( Style(..), styles )
import           Floskell.Types

import           Language.Haskell.Exts
                 hiding ( Comment, Pretty, Style, parse, prettyPrint, style )
import qualified Language.Haskell.Exts as Exts

data CodeBlock = HaskellSource Int [Text] | CPPDirectives [Text]
    deriving ( Show, Eq )

trimBy :: (a -> Bool) -> [a] -> ([a], [a], [a])
trimBy f xs = (prefix, middle, suffix)
  where
    (prefix, xs') = span f xs

    (suffix', middle') = span f $ reverse xs'

    middle = reverse middle'

    suffix = reverse suffix'

findLinePrefix :: (Char -> Bool) -> [Text] -> Text
findLinePrefix _ [] = ""
findLinePrefix f (x : xs') = go (TL.takeWhile f x) xs'
  where
    go prefix xs = if all (prefix `TL.isPrefixOf`) xs
                   then prefix
                   else go (TL.take (TL.length prefix - 1) prefix) xs

findIndent :: (Char -> Bool) -> [Text] -> Text
findIndent _ [] = ""
findIndent f (x : xs') = go (TL.takeWhile f x) $ filter (not . TL.all f) xs'
  where
    go indent xs = if all (indent `TL.isPrefixOf`) xs
                   then indent
                   else go (TL.take (TL.length indent - 1) indent) xs

preserveVSpace :: Monad m => ([Text] -> m [Text]) -> [Text] -> m [Text]
preserveVSpace format input = do
    output <- format input'
    return $ prefix ++ output ++ suffix
  where
    (prefix, input', suffix) = trimBy TL.null input

preservePrefix :: Monad m => (Int -> [Text] -> m [Text]) -> [Text] -> m [Text]
preservePrefix format input = do
    output <- format (prefixLength prefix) input'
    return $ map (prefix <>) output
  where
    prefix = findLinePrefix allowed input

    input' = map (TL.drop $ TL.length prefix) input

    allowed c = c == ' ' || c == '\t' || c == '>'

    prefixLength = sum . map (\c -> if c == '\t' then 8 else 1) . TL.unpack

preserveIndent :: Monad m => (Int -> [Text] -> m [Text]) -> [Text] -> m [Text]
preserveIndent format input = do
    output <- format (prefixLength prefix) input'
    return $ map (prefix <>) output
  where
    prefix = findIndent allowed input

    input' = map (TL.drop $ TL.length prefix) input

    allowed c = c == ' ' || c == '\t'

    prefixLength = sum . map (\c -> if c == '\t' then 8 else 1) . TL.unpack

withReducedLineLength :: Int -> Config -> Config
withReducedLineLength offset config = config { cfgPenalty = penalty }
  where
    penalty = (cfgPenalty config) { penaltyMaxLineLength =
                                        penaltyMaxLineLength (cfgPenalty config)
                                        - offset
                                  }

-- | Format the given source.
reformat :: AppConfig -> Maybe FilePath -> Text -> Either String Text
reformat config mfilepath input = fmap (TL.intercalate "\n")
    . preserveVSpace (preservePrefix (reformatLines mode cfg)) $ TL.lines input
  where
    mode = case readExtensions $ TL.unpack input of
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

reformatLines :: ParseMode -> Config -> Int -> [Text] -> Either String [Text]
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
              -> [Text]
              -> Either String [Text]
reformatBlock mode config cpp indent lines =
    case parseModuleWithComments mode code of
        ParseOk (m, comments') ->
            let comments = map makeComment comments'
                ast = annotateWithComments m (mergeComments comments cpp)
            in
                case prettyPrint (pretty ast) config' of
                    Nothing -> Left "Printer failed with mzero call."
                    Just output -> Right $ TL.lines output
        ParseFailed loc e -> Left $
            Exts.prettyPrint (loc { srcLine = srcLine loc }) ++ ": " ++ e
  where
    code = TL.unpack $ TL.intercalate "\n" lines

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
filterPreprocessorDirectives :: [Text] -> ([Text], [Comment])
filterPreprocessorDirectives lines = (code, comments)
  where
    code = map (\l -> if cppLine l then "" else l) lines

    comments = map makeComment . filter (cppLine . snd) $ zip [ 1 .. ] lines

    makeComment (n, l) =
        Comment PreprocessorDirective
                (SrcSpan "" n 1 n (fromIntegral $ TL.length l + 1))
                (TL.unpack l)

    cppLine src =
        any (`TL.isPrefixOf` src)
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

prettyPrint :: Printer a -> Config -> Maybe Text
prettyPrint printer = fmap (Buffer.toLazyText . psBuffer . snd)
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

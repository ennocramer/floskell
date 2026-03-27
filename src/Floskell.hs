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

import           Control.Monad          ( guard )
import           Data.List
import           Data.Maybe
#if __GLASGOW_HASKELL__ <= 802
import           Data.Monoid
#endif
import           Data.Char             ( isSpace )
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
    return $ map (\l -> if TL.null l then l else prefix <> l) output
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
    . preserveVSpace (preservePrefix (reformatLines mode cfg)) $
    TL.split (== '\n') input
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
reformatLines mode config indent = preserveVSpace (preserveIndent format)
  where
    format indent' =
        reformatBlock mode (withReducedLineLength (indent + indent') config)
        . filterCommentLike

-- | Format a continuous block of code without CPP directives.
reformatBlock
    :: ParseMode -> Config -> ([Text], [Comment]) -> Either String [Text]
reformatBlock mode config (lines, cpp) =
    case parseModuleWithComments mode code of
        ParseOk (m, comments') ->
            let comments = map makeComment comments'
                ast = annotateWithComments m (mergeComments comments cpp)
            in
                case prettyPrint (pretty ast) config of
                    Nothing -> Left "Printer failed with mzero call."
                    Just output -> Right $ TL.lines output
        ParseFailed loc e -> Left $
            Exts.prettyPrint (loc { srcLine = srcLine loc }) ++ ": " ++ e
  where
    code = TL.unpack $ TL.intercalate "\n" $ map rewriteImportQualifiedPost lines

    makeComment (Exts.Comment inline span text) =
        Comment (if inline then InlineComment else LineComment) span text

    mergeComments xs [] = xs
    mergeComments [] ys = ys
    mergeComments xs@(x : xs') ys@(y : ys') =
        if srcSpanStartLine (commentSpan x) < srcSpanStartLine (commentSpan y)
        then x : mergeComments xs' ys
        else y : mergeComments xs ys'

rewriteImportQualifiedPost :: Text -> Text
rewriteImportQualifiedPost = TL.pack . rewriteImportQualifiedPostString . TL.unpack

rewriteImportQualifiedPostString :: String -> String
rewriteImportQualifiedPostString line = case findPostQualifiedImport line of
    Just (moduleToken, qualifiedToken) ->
        swapTokens moduleToken qualifiedToken line
    Nothing -> line

findPostQualifiedImport :: String -> Maybe (ImportToken, ImportToken)
findPostQualifiedImport line = do
    let tokens = tokenize line
    (importToken, rest) <- uncons tokens
    guard $ tokenText importToken == "import"
    let rest' = skipImportModifiers rest
    (moduleToken, afterModule) <- uncons rest'
    qualifiedToken <- listToMaybe afterModule
    guard $ tokenText qualifiedToken == "qualified"
    return (moduleToken, qualifiedToken)

skipImportModifiers :: [ImportToken] -> [ImportToken]
skipImportModifiers
    ( ImportToken _ _ "{-#"
    : ImportToken _ _ "SOURCE"
    : ImportToken _ _ "#-}"
    : xs
    ) =
    skipImportModifiers xs
skipImportModifiers (tok : xs)
    | tokenText tok == "safe" = skipImportModifiers xs
    | isPackageToken tok = skipImportModifiers xs
skipImportModifiers xs = xs

isPackageToken :: ImportToken -> Bool
isPackageToken tok = case tokenText tok of
    '"' : _ -> True
    _ -> False

swapTokens :: ImportToken -> ImportToken -> String -> String
swapTokens moduleToken qualifiedToken line =
    prefix ++ tokenText qualifiedToken ++ middle ++ tokenText moduleToken ++ suffix
  where
    prefix = take (tokenStart moduleToken) line
    middle = take (tokenStart qualifiedToken - tokenEnd moduleToken)
        $ drop (tokenEnd moduleToken) line
    suffix = drop (tokenEnd qualifiedToken) line

data ImportToken = ImportToken
    { tokenStart :: Int
    , tokenEnd :: Int
    , tokenText :: String
    }

tokenize :: String -> [ImportToken]
tokenize = go 0
  where
    go _ [] = []
    go i xs@(x : xs')
        | isSpace x = go (i + 1) xs'
        | x == '"' =
            let (tok, rest) = spanString xs
                len = length tok
            in
                ImportToken i (i + len) tok : go (i + len) rest
        | otherwise =
            let (tok, rest) = break isSpace xs
                len = length tok
            in
                ImportToken i (i + len) tok : go (i + len) rest

    spanString [] = ([], [])
    spanString (x : xs) = firstChar [x] xs

    firstChar acc [] = (reverse acc, [])
    firstChar acc (x : xs)
        | x == '"' = (reverse (x : acc), xs)
        | otherwise = firstChar (x : acc) xs

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

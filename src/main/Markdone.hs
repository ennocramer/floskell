{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A subset of markdown that only supports @#headings@ and code
-- fences.
--
-- All content must be in section headings with proper hierarchy,
-- anything else is rejected.
module Markdone where

import           Control.DeepSeq
import           Control.Monad.Catch

import           Data.Char
import           Data.Monoid            ( (<>) )
import           Data.Text.Lazy         ( Text )
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO      as TIO
import           Data.Typeable

import           GHC.Generics

-- | A markdone token.
data Token = Heading !Int !Text | PlainLine !Text | BeginFence !Text | EndFence
    deriving ( Show )

-- | A markdone document.
data Markdone =
    Section !Text ![Markdone] | CodeFence !Text !Text | PlainText !Text
    deriving ( Show, Generic )

instance NFData Markdone

-- | Parse error.
data MarkdownError = NoFenceEnd | ExpectedSection
    deriving ( Typeable, Show )

instance Exception MarkdownError

-- | Tokenize the bytestring.
tokenize :: Text -> [Token]
tokenize = map token . TL.lines
  where
    token line
        | TL.isPrefixOf "#" line = let (hashes, title) = TL.span (== '#') line
                                   in
                                       Heading (fromIntegral $ TL.length hashes)
                                               (TL.dropWhile isSpace title)
        | TL.isPrefixOf "```" line =
            if line == "```"
            then EndFence
            else BeginFence (TL.dropWhile (\c -> c == '`' || c == ' ') line)
        | otherwise = PlainLine line

-- | Parse into a forest.
parse :: MonadThrow m => [Token] -> m [Markdone]
parse = go (0 :: Int)
  where
    go level = \case
        (Heading n label : rest) ->
            let (children, rest') = span (\case
                                              Heading nextN _ -> nextN > n
                                              _ -> True)
                                         rest
            in
                do
                    childs <- go (level + 1) children
                    siblings <- go level rest'
                    return (Section label childs : siblings)
        (BeginFence label : rest)
            | level > 0 ->
                let (content, rest') = span (\case
                                                 PlainLine{} -> True
                                                 _ -> False)
                                            rest
                in
                    case rest' of
                        (EndFence : rest'') ->
                            fmap (CodeFence label
                                            (TL.intercalate "\n"
                                                            (map getPlain
                                                                 content)) :)
                                 (go level rest'')
                        _ -> throwM NoFenceEnd
        PlainLine p : rest
            | level > 0 ->
                let (content, rest') = span (\case
                                                 PlainLine{} -> True
                                                 _ -> False)
                                            (PlainLine p : rest)
                in
                    fmap (PlainText (TL.intercalate "\n" (map getPlain content)) :)
                         (go level rest')
        [] -> return []
        _ -> throwM ExpectedSection

    getPlain (PlainLine x) = x
    getPlain _ = ""

print :: [Markdone] -> TB.Builder
print = mconcat . map (go 0)
  where
    go level = \case
        (Section heading children) ->
            let level' = level + 1
            in
                TB.fromLazyText (TL.replicate level' "#") <> TB.singleton ' '
                <> TB.fromLazyText heading <> TB.fromString "\n"
                <> mconcat (map (go level') children)
        (CodeFence lang code) ->
            TB.fromString "``` " <> TB.fromLazyText lang <> TB.singleton '\n'
            <> TB.fromLazyText code <> TB.fromString "\n```\n"
        (PlainText text) -> TB.fromLazyText text <> TB.fromString "\n"

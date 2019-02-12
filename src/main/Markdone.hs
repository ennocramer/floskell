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

import           Data.ByteString.Builder    as B
import qualified Data.ByteString.Char8      as S8
import           Data.ByteString.Lazy       ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Monoid                ( (<>) )
import           Data.Typeable

import           GHC.Generics

-- | A markdone token.
data Token = Heading !Int !ByteString
           | PlainLine !ByteString
           | BeginFence !ByteString
           | EndFence
    deriving ( Show )

-- | A markdone document.
data Markdone = Section !ByteString ![Markdone]
              | CodeFence !ByteString !ByteString
              | PlainText !ByteString
    deriving ( Show, Generic )

instance NFData Markdone

-- | Parse error.
data MarkdownError = NoFenceEnd | ExpectedSection
    deriving ( Typeable, Show )

instance Exception MarkdownError

-- | Tokenize the bytestring.
tokenize :: ByteString -> [Token]
tokenize = map token . L8.lines
  where
    token line
        | L8.isPrefixOf "#" line = let (hashes, title) = L8.span (== '#') line
                                   in
                                       Heading (fromIntegral $ L8.length hashes)
                                               (L8.dropWhile isSpace title)
        | L8.isPrefixOf "```" line =
            if line == "```"
            then EndFence
            else BeginFence (L8.dropWhile (\c -> c == '`' || c == ' ') line)
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
                                            (L8.intercalate "\n"
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
                    fmap (PlainText (L8.intercalate "\n" (map getPlain content)) :)
                         (go level rest')
        [] -> return []
        _ -> throwM ExpectedSection

    getPlain (PlainLine x) = x
    getPlain _ = ""

print :: [Markdone] -> B.Builder
print = mconcat . map (go (0 :: Int))
  where
    go level = \case
        (Section heading children) ->
            let level' = level + 1
            in
                B.byteString (S8.replicate level' '#') <> B.char7 ' '
                <> B.lazyByteString heading <> B.byteString "\n"
                <> mconcat (map (go level') children)
        (CodeFence lang code) -> B.byteString "``` " <> B.lazyByteString lang
            <> B.char7 '\n' <> B.lazyByteString code <> B.byteString "\n```\n"
        (PlainText text) -> B.lazyByteString text <> B.byteString "\n"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

-- | Benchmark the pretty printer.
module Main where

import           Control.DeepSeq

import           Criterion
import           Criterion.Main

import qualified Data.ByteString              as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.ByteString.Lazy.Builder as L
import qualified Data.ByteString.UTF8         as UTF8
import qualified Data.Text                    as T

import           Floskell

import           Markdone

-- | Main benchmarks.
main :: IO ()
main = do
    bytes <- S.readFile "BENCHMARK.md"
    !forest <- fmap force (parse (tokenize bytes))
    defaultMain [ bgroup (T.unpack $ styleName style) $ toCriterion style
                                                                    forest
                | style <- styles ]

-- | Convert the Markdone document to Criterion benchmarks.
toCriterion :: Style -> [Markdone] -> [Benchmark]
toCriterion style = go
  where
    go (Section name children : next) = bgroup (S8.unpack name) (go children) : go next
    go (PlainText desc : CodeFence lang code : next) =
        if lang == "haskell"
        then (bench (UTF8.toString desc)
                    (nf (either error L.toLazyByteString .
                             reformat style
                                      (Just defaultExtensions)
                                      (Just "BENCHMARK.md"))
                        code)) :
            go next
        else go next
    go (PlainText{} : next) = go next
    go (CodeFence{} : next) = go next
    go [] = []

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark the pretty printer.
module Main where

import           Control.DeepSeq

import           Criterion
import           Criterion.Main

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as UTF8
import qualified Data.Text                  as T

import           Floskell

import           Language.Haskell.Exts      ( Language(Haskell2010) )

import           Markdone

-- | Main benchmarks.
main :: IO ()
main = do
    bytes <- L8.readFile "BENCHMARK.md"
    !forest <- fmap force (parse (tokenize bytes))
    defaultMain [ bgroup (T.unpack $ styleName style) $
                    toCriterion (AppConfig style
                                           Haskell2010
                                           defaultExtensions
                                           [])
                                forest
                | style <- styles
                ]

-- | Convert the Markdone document to Criterion benchmarks.
toCriterion :: AppConfig -> [Markdone] -> [Benchmark]
toCriterion config = go
  where
    go (Section name children : next) = bgroup (L8.unpack name) (go children)
        : go next
    go (PlainText desc : CodeFence lang code : next) =
        if lang == "haskell"
        then bench (UTF8.toString desc)
                   (nf (either error id . reformat config (Just "BENCHMARK.md"))
                       code) : go next
        else go next
    go (PlainText{} : next) = go next
    go (CodeFence{} : next) = go next
    go [] = []

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark the pretty printer.
module Main where

import           Control.DeepSeq

import           Criterion
import           Criterion.Main

import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Data.Text.Lazy.IO     as TIO

import           Floskell

import           Language.Haskell.Exts ( Language(Haskell2010) )

import           Markdone

-- | Main benchmarks.
main :: IO ()
main = do
    bytes <- TIO.readFile "BENCHMARK.md"
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
    go (Section name children : next) = bgroup (TL.unpack name) (go children)
        : go next
    go (PlainText desc : CodeFence lang code : next) =
        if lang == "haskell"
        then bench (TL.unpack desc)
                   (nf (either error id . reformat config (Just "BENCHMARK.md"))
                       code) : go next
        else go next
    go (PlainText{} : next) = go next
    go (CodeFence{} : next) = go next
    go [] = []

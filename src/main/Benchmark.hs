{-# LANGUAGE BangPatterns #-}

-- | Benchmark the pretty printer.

module Main where

import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Builder as L
import           HIndent

-- | Main benchmarks.
main :: IO ()
main =
  defaultMain
    [env setupEnv
         (\ ~bigDecls ->
             bgroup "Main"
                    [bgroup "BigDeclarations"
                            [bench ("HIndent.reformat: " ++
                                    show (styleName style))
                                   (nf (either error L.toLazyByteString .
                                        reformat style (Just defaultExtensions))
                                       bigDecls)|style <- styles]])]

-- | Setup the environment for the benchmarks.
setupEnv :: IO ByteString
setupEnv = do
  bigDecls <- S.readFile "benchmarks/BigDeclarations.hs"
  let !decls = force bigDecls
  return decls

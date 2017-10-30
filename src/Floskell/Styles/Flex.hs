{-# LANGUAGE OverloadedStrings #-}

module Floskell.Styles.Flex ( flex ) where

import           Control.Monad.State.Strict   ( gets )

import           Data.Int                     ( Int64 )

import           Floskell.Flex.Config
import           Floskell.Flex.Pretty
import           Floskell.Types

import           Language.Haskell.Exts.Syntax

-- | Style definition.
flex :: Style
flex = Style { styleName = "flex"
             , styleAuthor = "Enno Cramer"
             , styleDescription = "Configurable formatting style"
             , styleInitialState = defaultFlexConfig
             , styleExtenders = [ Extender prettyModule ]
             , styleDefConfig = defaultConfig { configMaxColumns = 80
                                              , configIndentSpaces = 4
                                              }
             , styleCommentPreprocessor = return
             , styleLinePenalty = linePenalty
             }

-- | Entry Point for old pretty print framework
prettyModule :: PrettyPrinter Module
prettyModule = pretty

-- | Line penalty calculation
linePenalty :: Bool -> Int64 -> Printer FlexConfig Penalty
linePenalty eol col = do
    indent <- gets psIndentLevel
    maxcol <- gets (configMaxColumns . psConfig)
    config <- gets (cfgPenalty . psUserState)
    let pLinebreak = onlyIf eol $ penaltyLinebreak config
    let pIndent = fromIntegral indent * (penaltyIndent config)
    let pOverfull = onlyIf (col > maxcol) $
            penaltyOverfull config * fromIntegral (col - maxcol) + penaltyOverfullOnce config
    return . fromIntegral $ pLinebreak + pIndent + pOverfull
  where
    onlyIf cond penalty = if cond then penalty else 0

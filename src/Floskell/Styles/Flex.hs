{-# LANGUAGE OverloadedStrings #-}

module Floskell.Styles.Flex ( flex ) where

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
             , styleLinePenalty = defaultLinePenalty
             }

-- | Entry Point for old pretty print framework
prettyModule :: PrettyPrinter Module
prettyModule = pretty

{-# LANGUAGE DefaultSignatures #-}

module Floskell.Flex.Pretty where

import           Floskell.Flex.Config
import           Floskell.Flex.Printers

import           Floskell.Pretty              ( printComments )
import           Floskell.Types

import qualified Language.Haskell.Exts.Pretty as HSE
import           Language.Haskell.Exts.Syntax

-- | Syntax shortcut for Pretty Printers.
type PrettyPrinter f = f NodeInfo -> Printer FlexConfig ()

-- | Pretty printing prettyHSE using haskell-src-exts pretty printer
prettyHSE :: HSE.Pretty (ast NodeInfo) => PrettyPrinter ast
prettyHSE ast = string $ HSE.prettyPrint ast

-- | Type class for pretty-printable types.
class Pretty ast where
    prettyPrint :: PrettyPrinter ast
    default prettyPrint :: HSE.Pretty (ast NodeInfo) => PrettyPrinter ast
    prettyPrint = prettyHSE

-- | Pretty print a syntax tree with annotated comments
pretty :: (Annotated ast, Pretty ast) => PrettyPrinter ast
pretty ast = do
    printComments Before ast
    prettyPrint ast
    printComments After ast

instance Pretty Module

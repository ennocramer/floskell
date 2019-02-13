{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.
module Floskell.Types
    ( OutputRestriction(..)
    , Penalty(..)
    , TabStop(..)
    , Printer(..)
    , execPrinter
    , runPrinter
    , PrintState(..)
    , psLine
    , psColumn
    , psNewline
    , initialPrintState
    , Config(..)
    , SrcSpan(..)
    , Comment(..)
    , commentSpan
    , NodeInfo(..)
    , noNodeInfo
    , nodeSpan
    , Location(..)
    ) where

import           Control.Applicative
import           Control.Monad

import           Control.Monad.Search
                 ( MonadSearch, Search, runSearchBest )
import           Control.Monad.State.Strict
                 ( MonadState(..), StateT, execStateT, runStateT )

import qualified Data.Map.Strict                as Map

import           Data.Semigroup                 as Sem

import           Floskell.Buffer                ( Buffer )
import qualified Floskell.Buffer                as Buffer
import           Floskell.Config                ( Config(..), Location(..) )

import           Language.Haskell.Exts.Comments ( Comment(..) )
import           Language.Haskell.Exts.SrcLoc
                 ( SrcSpan(..), mkSrcSpan, noLoc )
import           Language.Haskell.Exts.Syntax   ( Annotated(..) )

data OutputRestriction = Anything | NoOverflow | NoOverflowOrLinebreak
    deriving ( Eq, Ord, Show )

newtype Penalty = Penalty Int
    deriving ( Eq, Ord, Num, Show )

newtype TabStop = TabStop String
    deriving ( Eq, Ord, Show )

instance Sem.Semigroup Penalty where
    (<>) = (+)

instance Monoid Penalty where
    mempty = 0
#if !(MIN_VERSION_base(4,11,0))

    mappend = (<>)
#endif

-- | A pretty printing monad.
newtype Printer a =
    Printer { unPrinter :: StateT PrintState (Search Penalty) a }
    deriving ( Applicative, Monad, Functor, MonadState PrintState
             , MonadSearch Penalty, MonadPlus, Alternative )

execPrinter :: Printer a -> PrintState -> Maybe (Penalty, PrintState)
execPrinter m s = runSearchBest $ execStateT (unPrinter m) s

runPrinter :: Printer a -> PrintState -> Maybe (Penalty, (a, PrintState))
runPrinter m s = runSearchBest $ runStateT (unPrinter m) s

-- | The state of the pretty printer.
data PrintState =
    PrintState { psBuffer :: !Buffer -- ^ Output buffer
               , psIndentLevel :: !Int -- ^ Current indentation level.
               , psOnside :: !Int -- ^ Extra indentation is necessary with next line break.
               , psTabStops :: !(Map.Map TabStop Int) -- ^ Tab stops for alignment.
               , psConfig :: !Config -- ^ Style definition.
               , psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
               , psOutputRestriction :: OutputRestriction
               }

psLine :: PrintState -> Int
psLine = Buffer.line . psBuffer

psColumn :: PrintState -> Int
psColumn = Buffer.column . psBuffer

psNewline :: PrintState -> Bool
psNewline = (== 0) . Buffer.column . psBuffer

commentSpan :: Comment -> SrcSpan
commentSpan (Comment _ ss _) = ss

initialPrintState :: Config -> PrintState
initialPrintState config =
    PrintState Buffer.empty 0 0 Map.empty config False Anything

-- | Information for each node in the AST.
data NodeInfo =
    NodeInfo { nodeInfoSpan :: !SrcSpan               -- ^ Location info from the parser.
             , nodeInfoLeadingComments :: ![Comment]  -- ^ Leading comments attached to this node.
             , nodeInfoTrailingComments :: ![Comment] -- ^ Trailing comments attached to this node.
             }
    deriving ( Show )

-- | Empty NodeInfo
noNodeInfo :: NodeInfo
noNodeInfo = NodeInfo (mkSrcSpan noLoc noLoc) [] []

nodeSpan :: Annotated ast => ast NodeInfo -> SrcSpan
nodeSpan = nodeInfoSpan . ann

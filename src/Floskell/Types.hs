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
    , Style(..)
    , Config(..)
    , NodeInfo(..)
    , ComInfo(..)
    , Location(..)
    ) where

import           Control.Applicative
import           Control.Monad

import           Control.Monad.Search
                 ( MonadSearch, Search, runSearchBest )
import           Control.Monad.State.Strict
                 ( MonadState(..), StateT, execStateT, runStateT )

import           Data.Int                       ( Int64 )
import           Data.Map.Strict                ( Map )

import           Data.Semigroup                 as Sem
import           Data.Text                      ( Text )

import           Floskell.Buffer                ( Buffer )
import qualified Floskell.Buffer                as Buffer
import           Floskell.Config                ( Config(..), Location(..) )

import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.SrcLoc

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
               , psIndentLevel :: !Int64 -- ^ Current indentation level.
               , psOnside :: !Int64 -- ^ Extra indentation is necessary with next line break.
               , psTabStops :: !(Map TabStop Int64) -- ^ Tab stops for alignment.
               , psUserState :: !Config -- ^ User state.
               , psEolComment :: !Bool -- ^ An end of line comment has just been outputted.
               , psOutputRestriction :: OutputRestriction
               }

psLine :: PrintState -> Int64
psLine = Buffer.line . psBuffer

psColumn :: PrintState -> Int64
psColumn = Buffer.column . psBuffer

psNewline :: PrintState -> Bool
psNewline = (== 0) . Buffer.column . psBuffer

-- | A printer style.
data Style =
    Style { styleName         :: !Text -- ^ Name of the style, used in the commandline interface.
          , styleAuthor       :: !Text -- ^ Author of the printer (as opposed to the author of the style).
          , styleDescription  :: !Text -- ^ Description of the style.
          , styleInitialState :: !Config -- ^ User state, if needed.
          }

-- | Information for each node in the AST.
data NodeInfo =
    NodeInfo { nodeInfoSpan     :: !SrcSpanInfo -- ^ Location info from the parser.
             , nodeInfoComments :: ![ComInfo] -- ^ Comments which are attached to this node.
             }
    deriving ( Show )

-- | Comment with some more info.
data ComInfo =
    ComInfo { comInfoComment  :: !Comment          -- ^ The normal comment type.
            , comInfoLocation :: !(Maybe Location) -- ^ Where the comment lies relative to the node.
            }
    deriving ( Show )

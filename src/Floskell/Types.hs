{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

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
    , Extender(..)
    , Style(..)
    , Config(..)
    , defaultConfig
    , NodeInfo(..)
    , ComInfo(..)
    , ComInfoLocation(..)
    ) where

import           Control.Applicative
import           Control.Monad

import           Control.Monad.Search           ( MonadSearch, Search
                                                , runSearchBest )
import           Control.Monad.State.Strict     ( MonadState(..), StateT
                                                , execStateT, runStateT )

import           Data.Data
import           Data.Int                       ( Int64 )
import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Semigroup                 as Sem

import           Floskell.Buffer                ( Buffer )
import qualified Floskell.Buffer                as Buffer

import           Language.Haskell.Exts.Comments
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.SrcLoc

data OutputRestriction = Anything | NoOverflow | NoOverflowOrLinebreak
    deriving (Eq, Ord, Show)

newtype Penalty = Penalty Int
    deriving (Eq, Ord, Num, Show)

newtype TabStop = TabStop String
    deriving (Eq, Ord, Show)

instance Sem.Semigroup Penalty where
    (<>) = (+)

instance Monoid Penalty where
    mempty = 0

#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

-- | A pretty printing monad.
newtype Printer s a =
    Printer { unPrinter :: StateT (PrintState s) (Search Penalty) a }
    deriving (Applicative, Monad, Functor, MonadState (PrintState s), MonadSearch Penalty, MonadPlus, Alternative)

execPrinter :: Printer s a -> PrintState s -> Maybe (Penalty, PrintState s)
execPrinter m s = runSearchBest $ execStateT (unPrinter m) s

runPrinter :: Printer s a -> PrintState s -> Maybe (Penalty, (a, PrintState s))
runPrinter m s = runSearchBest $ runStateT (unPrinter m) s

-- | The state of the pretty printer.
data PrintState s =
    PrintState { psBuffer              :: !Buffer -- ^ Output buffer
               , psIndentLevel         :: !Int64 -- ^ Current indentation level.
               , psOnside              :: !Int64 -- ^ Extra indentation is necessary with next line break.
               , psTabStops            :: !(Map TabStop Int64) -- ^ Tab stops for alignment.
               , psUserState           :: !s -- ^ User state.
               , psExtenders           :: ![Extender s] -- ^ Extenders.
               , psConfig              :: !Config -- ^ Config which styles may or may not pay attention to.
               , psEolComment          :: !Bool -- ^ An end of line comment has just been outputted.
               , psInsideCase          :: !Bool -- ^ Whether we're in a case statement, used for Rhs printing.
               , psParseMode           :: !ParseMode -- ^ Mode used to parse the original AST.
               , psCommentPreprocessor :: forall t.
                                       [Comment]
                                       -> Printer t [Comment] -- ^ Preprocessor applied to comments on an AST before printing.
               , psLinePenalty         :: Bool -> Int64 -> Printer s Penalty
               , psOutputRestriction   :: OutputRestriction
               }

psLine :: PrintState s -> Int64
psLine = Buffer.line . psBuffer

psColumn :: PrintState s -> Int64
psColumn = Buffer.column . psBuffer

psNewline :: PrintState s -> Bool
psNewline = (== 0) . Buffer.column . psBuffer

-- | A printer extender. Takes as argument the user state that the
-- printer was run with, and the current node to print. Use
-- 'prettyNoExt' to fallback to the built-in printer.
data Extender s where
        Extender ::
          forall s a . (Typeable a) => (a -> Printer s ()) -> Extender s
        CatchAll ::
          forall s .
            (forall a . Typeable a => s -> a -> Maybe (Printer s ())) ->
              Extender s

-- | A printer style.
data Style =
    forall s. Style { styleName                :: !Text -- ^ Name of the style, used in the commandline interface.
                    , styleAuthor              :: !Text -- ^ Author of the printer (as opposed to the author of the style).
                    , styleDescription         :: !Text -- ^ Description of the style.
                    , styleInitialState        :: !s -- ^ User state, if needed.
                    , styleExtenders           :: ![Extender s] -- ^ Extenders to the printer.
                    , styleDefConfig           :: !Config -- ^ Default config to use for this style.
                    , styleCommentPreprocessor :: forall t.
                                               [Comment]
                                               -> Printer t [Comment] -- ^ Preprocessor to use for comments.
                    , styleLinePenalty         :: Bool
                                               -> Int64
                                               -> Printer s Penalty
                    }

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
    Config { configMaxColumns      :: !Int64 -- ^ Maximum columns to fit code into ideally.
           , configIndentSpaces    :: !Int64 -- ^ How many spaces to indent?
           }

-- | Default style configuration.
defaultConfig :: Config
defaultConfig = Config { configMaxColumns = 80
                       , configIndentSpaces = 2
                       }

-- | Information for each node in the AST.
data NodeInfo =
    NodeInfo { nodeInfoSpan     :: !SrcSpanInfo -- ^ Location info from the parser.
             , nodeInfoComments :: ![ComInfo] -- ^ Comments which are attached to this node.
             }
    deriving (Typeable, Show, Data)

-- | Comment relative locations.
data ComInfoLocation = Before | After
    deriving (Show, Typeable, Data, Eq)

-- | Comment with some more info.
data ComInfo =
    ComInfo { comInfoComment  :: !Comment                -- ^ The normal comment type.
            , comInfoLocation :: !(Maybe ComInfoLocation) -- ^ Where the comment lies relative to the node.
            }
    deriving (Show, Typeable, Data)

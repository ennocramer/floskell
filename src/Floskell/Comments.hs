-- | Comment handling.
module Floskell.Comments ( annotateWithComments ) where

import           Control.Arrow                ( first, second )
import           Control.Monad.State.Strict

import           Data.Foldable                ( traverse_ )
import qualified Data.Map.Strict              as M

import           Floskell.Types

import           Language.Haskell.Exts.SrcLoc ( SrcSpanInfo(..) )

-- Order by start of span, larger spans before smaller spans.
newtype OrderByStart = OrderByStart SrcSpan
    deriving ( Eq )

instance Ord OrderByStart where
    compare (OrderByStart l) (OrderByStart r) =
        compare (srcSpanStartLine l) (srcSpanStartLine r)
        `mappend` compare (srcSpanStartColumn l) (srcSpanStartColumn r)
        `mappend` compare (srcSpanEndLine r) (srcSpanEndLine l)
        `mappend` compare (srcSpanEndColumn r) (srcSpanEndColumn l)

-- Order by end of span, smaller spans before larger spans.
newtype OrderByEnd = OrderByEnd SrcSpan
    deriving ( Eq )

instance Ord OrderByEnd where
    compare (OrderByEnd l) (OrderByEnd r) =
        compare (srcSpanEndLine l) (srcSpanEndLine r)
        `mappend` compare (srcSpanEndColumn l) (srcSpanEndColumn r)
        `mappend` compare (srcSpanStartLine r) (srcSpanStartLine l)
        `mappend` compare (srcSpanStartColumn r) (srcSpanStartColumn l)

onSameLine :: SrcSpan -> SrcSpan -> Bool
onSameLine ss ss' = srcSpanEndLine ss == srcSpanStartLine ss'

isAfterComment :: Comment -> Bool
isAfterComment (Comment _ _ str) =
    take 1 (dropWhile (== ' ') $ dropWhile (== '-') str) == "^"

isAlignedWith :: Comment -> Comment -> Bool
isAlignedWith (Comment _ before _) (Comment _ after _) =
    srcSpanEndLine before == srcSpanStartLine after - 1
    && srcSpanStartColumn before == srcSpanStartColumn after

-- | Annotate the AST with comments.
annotateWithComments :: Traversable ast
                     => ast SrcSpanInfo
                     -> [Comment]
                     -> ast NodeInfo
annotateWithComments src comments =
    evalState (do
                   traverse_ assignComment comments
                   traverse transferComments src)
              nodeinfos
  where
    nodeinfos :: M.Map SrcSpanInfo ([Comment], [Comment])
    nodeinfos = foldr (\ssi -> M.insert ssi ([], [])) M.empty src

    -- Assign a single comment to the right AST node
    assignComment :: Comment
                  -> State (M.Map SrcSpanInfo ([Comment], [Comment])) ()
    assignComment comment@(Comment _ cspan _) = case surrounding comment of
        (Nothing, Nothing) -> error "No target nodes for comment"
        (Just before, Nothing) -> insertComment After before
        (Nothing, Just after) -> insertComment Before after
        (Just before, Just after) ->
            if srcInfoSpan before `onSameLine` cspan || isAfterComment comment
            then insertComment After before
            else do
                cmts <- gets (M.! before)
                case cmts of
                    -- We've already collected comments for this
                    -- node and this comment is a continuation.
                    (_, c' : _)
                        | c' `isAlignedWith` comment ->
                            insertComment After before

                    -- The comment does not belong to this node.
                    -- If there is a node following this comment,
                    -- assign it to that node, else keep it here,
                    -- anyway.
                    _ -> insertComment Before after
      where
        insertComment :: Location
                      -> SrcSpanInfo
                      -> State (M.Map SrcSpanInfo ([Comment], [Comment])) ()
        insertComment Before ssi = modify $ M.adjust (first (comment :)) ssi
        insertComment After ssi = modify $ M.adjust (second (comment :)) ssi

    -- Transfer collected comments into the AST.
    transferComments
        :: SrcSpanInfo
        -> State (M.Map SrcSpanInfo ([Comment], [Comment])) NodeInfo
    transferComments ssi = do
        (c, c') <- gets (M.! ssi)
        -- Sometimes, there are multiple AST nodes with the same
        -- SrcSpan.  Make sure we assign comments to only one of
        -- them.
        modify $ M.insert ssi ([], [])
        return $ NodeInfo (srcInfoSpan ssi) (reverse c) (reverse c')

    surrounding (Comment _ ss _) = (nodeBefore ss, nodeAfter ss)

    nodeBefore ss = fmap snd $ OrderByEnd ss `M.lookupLT` spansByEnd

    nodeAfter ss = fmap snd $ OrderByStart ss `M.lookupGT` spansByStart

    spansByStart = foldr (\ssi -> M.insert (OrderByStart $ srcInfoSpan ssi) ssi)
                         M.empty
                         src

    spansByEnd =
        foldr (\ssi -> M.insert (OrderByEnd $ srcInfoSpan ssi) ssi) M.empty src

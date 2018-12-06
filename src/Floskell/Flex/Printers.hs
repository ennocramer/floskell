{-# LANGUAGE OverloadedStrings #-}

module Floskell.Flex.Printers
    ( getConfig
    , cut
    , oneline
      -- *
    , write
    , string
    , int
    , space
    , newline
    , linebreak
    , blankline
    , spaceOrNewline
      -- *
    , mayM_
    , withPrefix
    , withPostfix
    , withIndent
    , withLayout
    , inter
    , aligned
    , indented
    , onside
    , depend
    , depend'
    , parens
    , brackets
      -- *
    , group
    , groupH
    , groupV
      -- *
    , sepSpace
    , operator
    , operatorH
    , operatorV
    , alignOnOperator
    , alignOnOperatorH
    , alignOnOperatorV
    , withOperatorFormatting
    , withOperatorFormattingH
    , withOperatorFormattingV
    , comma
    ) where

import           Control.Applicative  ( (<|>) )
import           Control.Monad        ( when )

import           Data.ByteString      ( ByteString )
import           Data.List            ( intersperse )

import           Floskell.Flex.Config
import           Floskell.Pretty      ( brackets, cut, int, newline, parens
                                      , space, string, write )
import qualified Floskell.Pretty      as P
import           Floskell.Types

-- | Query part of the pretty printer config
getConfig :: (a -> b) -> Printer a b
getConfig f = f <$> P.getState

oneline :: Printer s a -> Printer s a
oneline = P.withOutputRestriction NoOverflowOrLinebreak

linebreak :: Printer s ()
linebreak = return () <|> newline

blankline :: Printer s ()
blankline = newline >> newline

spaceOrNewline :: Printer s ()
spaceOrNewline = space <|> newline

mayM_ :: Maybe a -> (a -> Printer s ()) -> Printer s ()
mayM_ Nothing _ = return ()
mayM_ (Just x) p = p x

withPrefix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPrefix pre f x = pre *> f x

withPostfix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPostfix post f x = f x <* post

withIndent :: (IndentConfig -> Indent)
           -> Printer FlexConfig a
           -> Printer FlexConfig a
withIndent fn p = do
    cfg <- getConfig (fn . cfgIndent)
    case cfg of
        Align -> align
        IndentBy i -> indentby i
        AlignOrIndentBy i -> align <|> indentby i
  where
    align = do
        space
        aligned p
    indentby indent = P.indented (fromIntegral indent) $ do
        newline
        p

withLayout :: (LayoutConfig -> Layout)
           -> Printer FlexConfig a
           -> Printer FlexConfig a
           -> Printer FlexConfig a
withLayout fn flex vertical = do
    cfg <- getConfig (fn . cfgLayout)
    case cfg of
        Flex -> flex
        Vertical -> vertical
        TryOneline -> oneline flex <|> vertical

inter :: Printer s () -> [Printer s ()] -> Printer s ()
inter x = sequence_ . intersperse x

aligned :: Printer s a -> Printer s a
aligned p = do
    col <- P.getNextColumn
    P.column col p

indented :: Printer FlexConfig a -> Printer FlexConfig a
indented p = do
    indent <- getConfig (cfgIndentOnside . cfgIndent)
    P.indented (fromIntegral indent) p

onside :: Printer FlexConfig a -> Printer FlexConfig a
onside p = do
    indent <- getConfig (cfgIndentOnside . cfgIndent)
    P.onside (fromIntegral indent) p

depend :: ByteString -> Printer FlexConfig a -> Printer FlexConfig a
depend kw = depend' (write kw)

depend' :: Printer FlexConfig ()
        -> Printer FlexConfig a
        -> Printer FlexConfig a
depend' kw p = do
    kw
    space
    indented p

group :: LayoutContext
      -> ByteString
      -> ByteString
      -> Printer FlexConfig ()
      -> Printer FlexConfig ()
group ctx open close p = do
    force <- getConfig (wsForceLinebreak . cfgGroupWs ctx open . cfgGroup)
    if force then vert else oneline hor <|> vert
  where
    hor = groupH ctx open close p
    vert = groupV ctx open close p

groupH :: LayoutContext
       -> ByteString
       -> ByteString
       -> Printer FlexConfig ()
       -> Printer FlexConfig ()
groupH ctx open close p = do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    when (wsSpace Before ws) space
    p
    when (wsSpace After ws) space
    write close

groupV :: LayoutContext
       -> ByteString
       -> ByteString
       -> Printer FlexConfig ()
       -> Printer FlexConfig ()
groupV ctx open close p = aligned $ do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    p
    if wsLinebreak After ws then newline else when (wsSpace After ws) space
    write close

sepSpace :: Printer FlexConfig ()
sepSpace = do
    sp <- getConfig (wsSpaces . cfgMapDefault . unOpConfig . cfgOp)
    when (sp /= WsNone) space

operator :: LayoutContext -> ByteString -> Printer FlexConfig ()
operator ctx op = alignOnOperator ctx op $ return ()

operatorH :: LayoutContext -> ByteString -> Printer FlexConfig ()
operatorH ctx op = alignOnOperatorH ctx op $ return ()

operatorV :: LayoutContext -> ByteString -> Printer FlexConfig ()
operatorV ctx op = alignOnOperatorV ctx op $ return ()

alignOnOperator :: LayoutContext
                -> ByteString
                -> Printer FlexConfig a
                -> Printer FlexConfig a
alignOnOperator ctx op = withOperatorFormatting ctx op (write op)

alignOnOperatorH :: LayoutContext
                 -> ByteString
                 -> Printer FlexConfig a
                 -> Printer FlexConfig a
alignOnOperatorH ctx op = withOperatorFormattingH ctx op (write op)

alignOnOperatorV :: LayoutContext
                 -> ByteString
                 -> Printer FlexConfig a
                 -> Printer FlexConfig a
alignOnOperatorV ctx op = withOperatorFormattingV ctx op (write op)

withOperatorFormatting :: LayoutContext
                       -> ByteString
                       -> Printer FlexConfig ()
                       -> Printer FlexConfig a
                       -> Printer FlexConfig a
withOperatorFormatting ctx op opp p = do
    force <- getConfig (wsForceLinebreak . cfgOpWs ctx op . cfgOp)
    if force then vert else hor <|> vert
  where
    hor = withOperatorFormattingH ctx op opp p
    vert = withOperatorFormattingV ctx op opp p

withOperatorFormattingH :: LayoutContext
                        -> ByteString
                        -> Printer FlexConfig ()
                        -> Printer FlexConfig a
                        -> Printer FlexConfig a
withOperatorFormattingH ctx op opp p = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    when (wsSpace Before ws) space
    aligned $ do
        opp
        when (wsSpace After ws) space
        p

withOperatorFormattingV :: LayoutContext
                        -> ByteString
                        -> Printer FlexConfig ()
                        -> Printer FlexConfig a
                        -> Printer FlexConfig a
withOperatorFormattingV ctx op opp p = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    aligned $ do
        opp
        if wsLinebreak After ws then newline else when (wsSpace After ws) space
        p

comma :: Printer FlexConfig ()
comma = operator Expression ","

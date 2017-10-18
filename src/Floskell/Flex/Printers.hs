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
    , inter
    , aligned
    , indented
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

inter :: Printer s () -> [Printer s ()] -> Printer s ()
inter x = sequence_ . intersperse x

aligned :: Printer s a -> Printer s a
aligned p = do
    col <- P.getNextColumn
    P.column col p

indented :: Printer FlexConfig a -> Printer FlexConfig a
indented = P.indentedBlock

depend :: ByteString -> Printer FlexConfig a -> Printer FlexConfig a
depend kw = depend' (write kw)

depend' :: Printer FlexConfig () -> Printer FlexConfig a -> Printer FlexConfig a
depend' kw p = do
    kw
    space
    indented p

group :: ByteString
      -> ByteString
      -> Printer FlexConfig ()
      -> Printer FlexConfig ()
group open close p = do
    force <- getConfig (wsForceLinebreak . cfgGroupWs open . cfgGroup)
    if force then vert else oneline hor <|> vert
  where
    hor = groupH open close p
    vert = groupV open close p

groupH :: ByteString
       -> ByteString
       -> Printer FlexConfig ()
       -> Printer FlexConfig ()
groupH open close p = do
    ws <- getConfig (cfgGroupWs open . cfgGroup)
    write open
    when (wsSpace Before ws) space
    p
    when (wsSpace After ws) space
    write close

groupV :: ByteString
       -> ByteString
       -> Printer FlexConfig ()
       -> Printer FlexConfig ()
groupV open close p = aligned $ do
    ws <- getConfig (cfgGroupWs open . cfgGroup)
    write open
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    p
    if wsLinebreak After ws then newline else when (wsSpace After ws) space
    write close

sepSpace :: Printer FlexConfig ()
sepSpace = do
    sp <- getConfig (wsSpaces . cfgOpWsDefault . cfgOp)
    when (sp /= WsNone) space

operator :: ByteString -> Printer FlexConfig ()
operator op = alignOnOperator op $ return ()

operatorH :: ByteString -> Printer FlexConfig ()
operatorH op = alignOnOperatorH op $ return ()

operatorV :: ByteString -> Printer FlexConfig ()
operatorV op = alignOnOperatorV op $ return ()

alignOnOperator :: ByteString -> Printer FlexConfig a -> Printer FlexConfig a
alignOnOperator op = withOperatorFormatting op (write op)

alignOnOperatorH :: ByteString -> Printer FlexConfig a -> Printer FlexConfig a
alignOnOperatorH op = withOperatorFormattingH op (write op)

alignOnOperatorV :: ByteString -> Printer FlexConfig a -> Printer FlexConfig a
alignOnOperatorV op = withOperatorFormattingV op (write op)

withOperatorFormatting :: ByteString
                       -> Printer FlexConfig ()
                       -> Printer FlexConfig a
                       -> Printer FlexConfig a
withOperatorFormatting op opp p = do
    force <- getConfig (wsForceLinebreak . cfgOpWs op . cfgOp)
    if force then vert else hor <|> vert
  where
    hor = withOperatorFormattingH op opp p
    vert = withOperatorFormattingV op opp p

withOperatorFormattingH :: ByteString
                        -> Printer FlexConfig ()
                        -> Printer FlexConfig a
                        -> Printer FlexConfig a
withOperatorFormattingH op opp p = do
    ws <- getConfig (cfgOpWs op . cfgOp)
    when (wsSpace Before ws) space
    aligned $ do
        opp
        when (wsSpace After ws) space
        p

withOperatorFormattingV :: ByteString
                        -> Printer FlexConfig ()
                        -> Printer FlexConfig a
                        -> Printer FlexConfig a
withOperatorFormattingV op opp p = do
    ws <- getConfig (cfgOpWs op . cfgOp)
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    aligned $ do
        opp
        if wsLinebreak After ws then newline else when (wsSpace After ws) space
        p

comma :: Printer FlexConfig ()
comma = operator ","

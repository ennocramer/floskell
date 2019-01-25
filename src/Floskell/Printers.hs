{-# LANGUAGE OverloadedStrings #-}

module Floskell.Printers
    ( getConfig
    , getOption
    , cut
    , oneline
      -- * Basic printing
    , write
    , string
    , int
    , space
    , newline
    , linebreak
    , blankline
    , spaceOrNewline
      -- * Tab stops
    , withTabStops
    , atTabStop
      -- * Combinators
    , mayM_
    , withPrefix
    , withPostfix
    , withIndent
    , withIndentFlat
    , withIndentBy
    , withLayout
    , inter
      -- * Indentation
    , getNextColumn
    , column
    , aligned
    , indented
    , onside
    , depend
    , depend'
    , parens
    , brackets
      -- * Wrapping
    , group
    , groupH
    , groupV
      -- * Operators
    , operator
    , operatorH
    , operatorV
    , alignOnOperator
    , alignOnOperatorH
    , alignOnOperatorV
    , withOperatorFormatting
    , withOperatorFormattingH
    , withOperatorFormattingV
    , operatorSectionL
    , operatorSectionR
    , comma
    ) where

import           Control.Applicative        ( (<|>) )

import           Control.Monad              ( guard, unless, when )
import           Control.Monad.Search       ( cost, winner )

import           Control.Monad.State.Strict ( get, gets, modify )

import           Data.ByteString            ( ByteString )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.Int                   ( Int64 )
import           Data.List                  ( intersperse )
import qualified Data.Map.Strict            as Map
import           Data.Monoid                ( (<>) )

import qualified Floskell.Buffer            as Buffer
import           Floskell.Config
import           Floskell.Types

-- | Query part of the pretty printer config
getConfig :: (Config -> b) -> Printer b
getConfig f = f <$> gets psUserState

-- | Query pretty printer options
getOption :: (OptionConfig -> Bool) -> Printer Bool
getOption f = getConfig (f . cfgOptions)

-- | Line penalty calculation
linePenalty :: Bool -> Int64 -> Printer Penalty
linePenalty eol col = do
    indentLevel <- gets psIndentLevel
    config <- getConfig cfgPenalty
    let maxcol = penaltyMaxLineLength config
    let pLinebreak = onlyIf eol $ penaltyLinebreak config
    let pIndent = fromIntegral indentLevel * (penaltyIndent config)
    let pOverfull = onlyIf (col > fromIntegral maxcol) $ penaltyOverfull config
            * fromIntegral (col - fromIntegral maxcol)
            + penaltyOverfullOnce config
    return . fromIntegral $ pLinebreak + pIndent + pOverfull
  where
    onlyIf cond penalty = if cond then penalty else 0

-- | Try only the first (i.e. locally best) solution to the given
-- pretty printer.  Use this function to improve performance whenever
-- the formatting of an AST node has no effect on the penalty of any
-- following AST node, such as top-level declarations or case
-- branches.
cut :: Printer a -> Printer a
cut = winner

withOutputRestriction :: OutputRestriction -> Printer a -> Printer a
withOutputRestriction r p = do
    orig <- gets psOutputRestriction
    modify $ \s -> s { psOutputRestriction = max orig r }
    result <- p
    modify $ \s -> s { psOutputRestriction = orig }
    return result

oneline :: Printer a -> Printer a
oneline p = do
    eol <- gets psEolComment
    when eol $ newline
    withOutputRestriction NoOverflowOrLinebreak p

-- | Write out a string, updating the current position information.
write :: ByteString -> Printer ()
write x = do
    eol <- gets psEolComment
    when eol newline
    write' x
  where
    write' x' = do
        state <- get
        let indentLevel = fromIntegral (psIndentLevel state)
            out = if psNewline state
                  then BS.replicate indentLevel 32 <> x'
                  else x'
            buffer = psBuffer state
            newCol = Buffer.column buffer + fromIntegral (BS.length out)
        guard $ psOutputRestriction state == Anything || newCol
            < fromIntegral (penaltyMaxLineLength (cfgPenalty (psUserState state)))
        penalty <- linePenalty False newCol
        when (penalty /= mempty) $ cost mempty penalty
        modify (\s ->
                s { psBuffer = Buffer.write out buffer, psEolComment = False })

-- | Write a string.
string :: String -> Printer ()
string = write . BL.toStrict . BB.toLazyByteString . BB.stringUtf8

-- | Write an integral.
int :: Integer -> Printer ()
int = string . show

-- | Write a space.
space :: Printer ()
space = do
    comment <- gets psEolComment
    unless comment $ write " "

-- | Output a newline.
newline :: Printer ()
newline = do
    modify (\s ->
            s { psIndentLevel = psIndentLevel s + psOnside s, psOnside = 0 })
    state <- get
    guard $ psOutputRestriction state /= NoOverflowOrLinebreak
    penalty <- linePenalty True (psColumn state)
    when (penalty /= mempty) $ cost penalty mempty
    modify (\s -> s { psBuffer     = Buffer.newline (psBuffer state)
                    , psEolComment = False
                    })

linebreak :: Printer ()
linebreak = return () <|> newline

blankline :: Printer ()
blankline = newline >> newline

spaceOrNewline :: Printer ()
spaceOrNewline = space <|> newline

withTabStops :: [(TabStop, Maybe Int)] -> Printer a -> Printer a
withTabStops stops p = do
    col <- getNextColumn
    oldstops <- gets psTabStops
    modify $ \s ->
        s { psTabStops =
                foldr (\(k, v) ->
                       Map.alter (const $ fmap (\x -> col + fromIntegral x) v)
                                 k)
                      (psTabStops s)
                      stops
          }
    res <- p
    modify $ \s -> s { psTabStops = oldstops }
    return res

atTabStop :: TabStop -> Printer ()
atTabStop tabstop = do
    mstop <- gets (Map.lookup tabstop . psTabStops)
    mayM_ mstop $ \stop -> do
        col <- getNextColumn
        let padding = max 0 $ fromIntegral (stop - col)
        write (BS.replicate padding 32)

mayM_ :: Maybe a -> (a -> Printer ()) -> Printer ()
mayM_ Nothing _ = return ()
mayM_ (Just x) p = p x

withPrefix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPrefix pre f x = pre *> f x

withPostfix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPostfix post f x = f x <* post

withIndent :: (IndentConfig -> Indent) -> Printer a -> Printer a
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

    indentby i = indent i $ do
        newline
        p

withIndentFlat :: (IndentConfig -> Indent)
               -> ByteString
               -> Printer a
               -> Printer a
withIndentFlat fn kw p = do
    cfg <- getConfig (fn . cfgIndent)
    case cfg of
        Align -> align
        IndentBy i -> indentby i
        AlignOrIndentBy i -> align <|> indentby i
  where
    align = aligned $ do
        write kw
        p

    indentby i = do
        write kw
        indent i p

withIndentBy :: (IndentConfig -> Int) -> Printer a -> Printer a
withIndentBy fn = withIndent (IndentBy . fn)

withLayout :: (LayoutConfig -> Layout) -> Printer a -> Printer a -> Printer a
withLayout fn flex vertical = do
    cfg <- getConfig (fn . cfgLayout)
    case cfg of
        Flex -> flex
        Vertical -> vertical
        TryOneline -> oneline flex <|> vertical

inter :: Printer () -> [Printer ()] -> Printer ()
inter x = sequence_ . intersperse x

-- | Get the column for the next printed character.
getNextColumn :: Printer Int64
getNextColumn = do
    st <- get
    return $ if psEolComment st
             then psIndentLevel st + psOnside st
             else max (psColumn st) (psIndentLevel st)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p = do
    level <- gets psIndentLevel
    onside' <- gets psOnside
    modify (\s -> s { psIndentLevel = i
                    , psOnside      = if i > level then 0 else onside'
                    })
    m <- p
    modify (\s -> s { psIndentLevel = level, psOnside = onside' })
    return m

-- | Increase indentation level by n spaces for the given printer.
indent :: Int -> Printer a -> Printer a
indent i p = do
    level <- gets psIndentLevel
    column (level + fromIntegral i) p

aligned :: Printer a -> Printer a
aligned p = do
    col <- getNextColumn
    column col $ do
        modify $ \s -> s { psOnside = 0 }
        p

indented :: Printer a -> Printer a
indented p = do
    i <- getConfig (cfgIndentOnside . cfgIndent)
    indent i p

-- | Increase indentation level b n spaces for the given printer, but
-- ignore increase when computing further indentations.
onside :: Printer a -> Printer a
onside p = do
    eol <- gets psEolComment
    when eol newline
    onsideIndent <- getConfig (cfgIndentOnside . cfgIndent)
    level <- gets psIndentLevel
    onside' <- gets psOnside
    modify (\s -> s { psOnside = fromIntegral onsideIndent })
    m <- p
    modify (\s -> s { psIndentLevel = level, psOnside = onside' })
    return m

depend :: ByteString -> Printer a -> Printer a
depend kw = depend' (write kw)

depend' :: Printer () -> Printer a -> Printer a
depend' kw p = do
    kw
    space
    indented p

-- | Wrap in parens.
parens :: Printer () -> Printer ()
parens p = do
    write "("
    aligned $ do
        p
        write ")"

-- | Wrap in brackets.
brackets :: Printer () -> Printer ()
brackets p = do
    write "["
    aligned $ do
        p
        write "]"

group :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
group ctx open close p = do
    force <- getConfig (wsForceLinebreak . cfgGroupWs ctx open . cfgGroup)
    if force then vert else oneline hor <|> vert
  where
    hor = groupH ctx open close p

    vert = groupV ctx open close p

groupH :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
groupH ctx open close p = do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    when (wsSpace Before ws) space
    p
    when (wsSpace After ws) space
    write close

groupV :: LayoutContext -> ByteString -> ByteString -> Printer () -> Printer ()
groupV ctx open close p = aligned $ do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    p
    if wsLinebreak After ws then newline else when (wsSpace After ws) space
    write close

operator :: LayoutContext -> ByteString -> Printer ()
operator ctx op = withOperatorFormatting ctx op (write op) id

operatorH :: LayoutContext -> ByteString -> Printer ()
operatorH ctx op = withOperatorFormattingH ctx op (write op) id

operatorV :: LayoutContext -> ByteString -> Printer ()
operatorV ctx op = withOperatorFormattingV ctx op (write op) id

alignOnOperator :: LayoutContext -> ByteString -> Printer a -> Printer a
alignOnOperator ctx op p =
    withOperatorFormatting ctx op (write op) (aligned . (*> p))

alignOnOperatorH :: LayoutContext -> ByteString -> Printer a -> Printer a
alignOnOperatorH ctx op p =
    withOperatorFormattingH ctx op (write op) (aligned . (*> p))

alignOnOperatorV :: LayoutContext -> ByteString -> Printer a -> Printer a
alignOnOperatorV ctx op p =
    withOperatorFormattingV ctx op (write op) (aligned . (*> p))

withOperatorFormatting :: LayoutContext
                       -> ByteString
                       -> Printer ()
                       -> (Printer () -> Printer a)
                       -> Printer a
withOperatorFormatting ctx op opp fn = do
    force <- getConfig (wsForceLinebreak . cfgOpWs ctx op . cfgOp)
    if force then vert else hor <|> vert
  where
    hor = withOperatorFormattingH ctx op opp fn

    vert = withOperatorFormattingV ctx op opp fn

withOperatorFormattingH :: LayoutContext
                        -> ByteString
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingH ctx op opp fn = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    nl <- gets psNewline
    eol <- gets psEolComment
    when (wsSpace Before ws && not nl && not eol) space
    fn $ do
        opp
        when (wsSpace After ws) space

withOperatorFormattingV :: LayoutContext
                        -> ByteString
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingV ctx op opp fn = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    nl <- gets psNewline
    eol <- gets psEolComment
    if wsLinebreak Before ws
        then newline
        else when (wsSpace Before ws && not nl && not eol) space
    fn $ do
        opp
        if wsLinebreak After ws then newline else when (wsSpace After ws) space

operatorSectionL :: LayoutContext -> ByteString -> Printer () -> Printer ()
operatorSectionL ctx op opp = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    when (wsSpace Before ws) space
    opp

operatorSectionR :: LayoutContext -> ByteString -> Printer () -> Printer ()
operatorSectionR ctx op opp = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    opp
    when (wsSpace After ws) space

comma :: Printer ()
comma = operator Expression ","

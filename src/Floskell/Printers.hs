{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Floskell.Printers
    ( getConfig
    , getOption
    , cut
    , closeEolComment
    , oneline
    , ignoreOneline
      -- * Basic printing
    , write
    , string
    , int
    , space
    , newline
    , ensureNewline
    , blankline
    , spaceOrNewline
      -- * Tab stops
    , withTabStops
    , atTabStop
      -- * Combinators
    , mayM_
    , withPrefix
    , withPostfix
    , withIndentConfig
    , withIndent
    , withIndentFlex
    , withIndentAfter
    , withIndentBy
    , withLayout
    , inter
      -- * Indentation
    , getNextColumn
    , column
    , aligned
    , indented
    , onside
    , suppressOnside
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

import           Data.List                  ( intersperse )
import qualified Data.Map.Strict            as Map
#if __GLASGOW_HASKELL__ <= 802
import           Data.Monoid                ( (<>) )
#endif
import           Data.Text                  ( Text )
import qualified Data.Text                  as T

import qualified Floskell.Buffer            as Buffer
import           Floskell.Config
import           Floskell.Types

-- | Query part of the pretty printer config
getConfig :: (Config -> b) -> Printer b
getConfig f = f <$> gets psConfig

-- | Query pretty printer options
getOption :: (OptionConfig -> a) -> Printer a
getOption f = getConfig (f . cfgOptions)

-- | Line penalty calculation
linePenalty :: Bool -> Int -> Printer Penalty
linePenalty eol col = do
    indentLevel <- gets psIndentLevel
    config <- getConfig cfgPenalty
    let maxcol = penaltyMaxLineLength config
    let pLinebreak = onlyIf eol $ penaltyLinebreak config
    let pIndent = indentLevel * penaltyIndent config
    let pOverfull = onlyIf (col > maxcol) $ penaltyOverfull config
            * (col - maxcol) + penaltyOverfullOnce config
    return . Penalty $ pLinebreak + pIndent + pOverfull
  where
    onlyIf cond penalty = if cond then penalty else 0

-- | Try only the first (i.e. locally best) solution to the given
-- pretty printer.  Use this function to improve performance whenever
-- the formatting of an AST node has no effect on the penalty of any
-- following AST node, such as top-level declarations or case
-- branches.
cut :: Printer a -> Printer a
cut = winner

closeEolComment :: Printer ()
closeEolComment = do
    eol <- gets psEolComment
    when eol newline

withOutputRestriction :: OutputRestriction -> Printer a -> Printer a
withOutputRestriction r p = do
    orig <- gets psOutputRestriction
    modify $ \s -> s { psOutputRestriction = r }
    result <- p
    modify $ \s -> s { psOutputRestriction = orig }
    return result

oneline :: Printer a -> Printer a
oneline p = do
    closeEolComment
    withOutputRestriction NoOverflowOrLinebreak p

ignoreOneline :: Printer a -> Printer a
ignoreOneline = withOutputRestriction Anything

-- | Write out a string, updating the current position information.
write :: Text -> Printer ()
write x = do
    closeEolComment
    write' x
  where
    write' x' = do
        state <- get
        let indentLevel = psIndentLevel state
            out = if psNewline state
                  then T.replicate indentLevel " " <> x'
                  else x'
            buffer = psBuffer state
            newCol = Buffer.column buffer + T.length out
        guard $ psOutputRestriction state == Anything || newCol
            < penaltyMaxLineLength (cfgPenalty (psConfig state))
        penalty <- linePenalty False newCol
        when (penalty /= mempty) $ cost mempty penalty
        modify (\s ->
                s { psBuffer = Buffer.write out buffer, psEolComment = False })

-- | Write a string.
string :: String -> Printer ()
string = write . T.pack

-- | Write an integral.
int :: Int -> Printer ()
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

-- | Output a newline if not at the start of a line
ensureNewline :: Printer ()
ensureNewline = do
    nl <- gets psNewline
    unless nl newline

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
                foldr (\(k, v) -> Map.alter (const $ fmap (\x -> col + x) v) k)
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
        let padding = max 0 (stop - col)
        write (T.replicate padding " ")

mayM_ :: Maybe a -> (a -> Printer ()) -> Printer ()
mayM_ Nothing _ = return ()
mayM_ (Just x) p = p x

withPrefix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPrefix pre f x = pre *> f x

withPostfix :: Applicative f => f a -> (x -> f b) -> x -> f b
withPostfix post f x = f x <* post

withIndentConfig
    :: (IndentConfig -> Indent) -> Printer a -> (Int -> Printer a) -> Printer a
withIndentConfig fn align indentby = do
    cfg <- getConfig (fn . cfgIndent)
    case cfg of
        Align -> align
        IndentBy i -> indentby i
        AlignOrIndentBy i -> align <|> indentby i

withIndent :: (IndentConfig -> Indent) -> Printer a -> Printer a
withIndent fn p = withIndentConfig fn align indentby
  where
    align = do
        space
        aligned p

    indentby i = indented i $ do
        newline
        p

withIndentFlex :: (IndentConfig -> Indent) -> Printer a -> Printer a
withIndentFlex fn p = withIndentConfig fn align indentby
  where
    align = do
        space
        aligned p

    indentby i = indented i $ do
        spaceOrNewline
        p

withIndentAfter
    :: (IndentConfig -> Indent) -> Printer () -> Printer a -> Printer a
withIndentAfter fn before p = withIndentConfig fn align indentby
  where
    align = aligned $ do
        withIndentation id before
        p

    indentby i = do
        withIndentation id before
        indented i p

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
getNextColumn :: Printer Int
getNextColumn = do
    st <- get
    return $ if psEolComment st
             then psIndentLevel st + psOnside st
             else max (psColumn st) (psIndentLevel st)

withIndentation :: ((Int, Int) -> (Int, Int)) -> Printer a -> Printer a
withIndentation f p = do
    prevIndent <- gets psIndentLevel
    prevOnside <- gets psOnside
    let (newIndent, newOnside) = f (prevIndent, prevOnside)
    modify (\s -> s { psIndentLevel = newIndent, psOnside = newOnside })
    r <- p
    modify (\s -> s { psIndentLevel = prevIndent, psOnside = prevOnside })
    return r

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int -> Printer a -> Printer a
column i = withIndentation $ \(l, o) -> (i, if i > l then 0 else o)

aligned :: Printer a -> Printer a
aligned p = do
    col <- getNextColumn
    column col p

-- | Increase indentation level by n spaces for the given printer.
indented :: Int -> Printer a -> Printer a
indented i p = do
    level <- gets psIndentLevel
    column (level + i) p

-- | Increase indentation level by n spaces for the given printer, but
-- ignore increase when computing further indentations.
onside :: Printer a -> Printer a
onside p = do
    closeEolComment
    onsideIndent <- getConfig (cfgIndentOnside . cfgIndent)
    withIndentation (\(l, _) -> (l, onsideIndent)) p

-- | Temporarily ignore any onside identation.
suppressOnside :: Printer () -> Printer ()
suppressOnside printer = do
    nl <- gets psNewline
    onsideIndent <- gets psOnside
    when nl $ modify $ \s -> s { psOnside = 0 }
    printer
    modify $ \s -> s { psOnside = onsideIndent }

depend :: Text -> Printer a -> Printer a
depend kw = depend' (write kw)

depend' :: Printer () -> Printer a -> Printer a
depend' kw p = do
    i <- getConfig (cfgIndentOnside . cfgIndent)
    kw
    space
    indented i p

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

group :: LayoutContext -> Text -> Text -> Printer () -> Printer ()
group ctx open close p = do
    force <- getConfig (wsForceLinebreak . cfgGroupWs ctx open . cfgGroup)
    if force then vert else oneline hor <|> vert
  where
    hor = groupH ctx open close p

    vert = groupV ctx open close p

groupH :: LayoutContext -> Text -> Text -> Printer () -> Printer ()
groupH ctx open close p = do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    when (wsSpace Before ws) space
    p
    when (wsSpace After ws) space
    write close

groupV :: LayoutContext -> Text -> Text -> Printer () -> Printer ()
groupV ctx open close p = aligned $ do
    ws <- getConfig (cfgGroupWs ctx open . cfgGroup)
    write open
    if wsLinebreak Before ws then newline else when (wsSpace Before ws) space
    p
    if wsLinebreak After ws then newline else when (wsSpace After ws) space
    write close

operator :: LayoutContext -> Text -> Printer ()
operator ctx op = withOperatorFormatting ctx op (write op) id

operatorH :: LayoutContext -> Text -> Printer ()
operatorH ctx op = withOperatorFormattingH ctx op (write op) id

operatorV :: LayoutContext -> Text -> Printer ()
operatorV ctx op = withOperatorFormattingV ctx op (write op) id

alignOnOperator :: LayoutContext -> Text -> Printer a -> Printer a
alignOnOperator ctx op p =
    withOperatorFormatting ctx op (write op) (aligned . (*> p))

withOperatorFormatting :: LayoutContext
                       -> Text
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
                        -> Text
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingH ctx op opp fn = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    when (wsSpace Before ws) space
    fn $ do
        opp
        when (wsSpace After ws) space

withOperatorFormattingV :: LayoutContext
                        -> Text
                        -> Printer ()
                        -> (Printer () -> Printer a)
                        -> Printer a
withOperatorFormattingV ctx op opp fn = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    if wsLinebreak Before ws
        then ensureNewline
        else when (wsSpace Before ws) space
    fn $ do
        opp
        if wsLinebreak After ws then newline else when (wsSpace After ws) space

operatorSectionL :: LayoutContext -> Text -> Printer () -> Printer ()
operatorSectionL ctx op opp = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    when (wsSpace Before ws) space
    opp

operatorSectionR :: LayoutContext -> Text -> Printer () -> Printer ()
operatorSectionR ctx op opp = do
    ws <- getConfig (cfgOpWs ctx op . cfgOp)
    opp
    when (wsSpace After ws) space

comma :: Printer ()
comma = operator Expression ","

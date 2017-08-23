{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | Pretty printing.
module Floskell.Pretty
    ( -- * Printing
      Pretty
    , pretty
    , prettyNoExt
    , cut
    , withOutputRestriction
    , fitsOnOneLineOr
      -- * User state
    , getState
    , putState
    , modifyState
      -- * Insertion
    , write
    , newline
    , space
    , comma
    , int
    , string
      -- * Common node types
    , maybeCtx
    , withCtx
    , printComment
    , printComments
    , withCaseContext
    , rhsSeparator
    , prettyInfixOp
      -- * Interspersing
    , inter
    , spaced
    , lined
    , prefixedLined
    , commas
      -- * Wrapping
    , parens
    , brackets
    , braces
      -- * Indentation
    , indented
    , indentedBlock
    , column
    , getColumn
    , getNextColumn
    , getLineNum
    , depend
    , dependBind
    , swing
    , swingBy
    , getIndentSpaces
    , getColumnLimit
      -- * Predicates
    , nullBinds
      -- * Sandboxing
    , sandbox
      -- * Fallback
    , pretty'
    ) where

import           Control.Applicative            ( (<|>), empty )

import           Control.Monad.Search           ( cost, cost' )
import           Control.Monad.State.Strict     hiding ( state )

import qualified Data.ByteString                as S
import qualified Data.ByteString.Builder        as S
import qualified Data.ByteString.Lazy           as S ( toStrict )

import           Data.Foldable                  ( traverse_ )
import           Data.Int
import           Data.List
import           Data.Maybe

import           Data.Monoid                    hiding ( Alt )
import           Data.Typeable

import qualified Floskell.Buffer                as Buffer
import           Floskell.Types

import qualified Language.Haskell.Exts          as P
import           Language.Haskell.Exts.Comments

import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax

import           Prelude                        hiding ( exp )

--------------------------------------------------------------------------------
-- * Pretty printing class
-- | Pretty printing class.
class (Annotated ast, Typeable ast) => Pretty ast where
    prettyInternal :: ast NodeInfo -> Printer s ()

-- | Pretty print using extenders.
pretty :: Pretty ast => ast NodeInfo -> Printer s ()
pretty a = do
    st <- get
    case st of
        PrintState{psExtenders = es,psUserState = s} -> do
            printComments Before a
            depend (fromMaybe (prettyNoExt a) $
                        listToMaybe (mapMaybe (makePrinter s) es))
                   (printComments After a)
  where
    makePrinter _ (Extender f) = case cast a of
        Just v -> Just (f v)
        Nothing -> Nothing
    makePrinter s (CatchAll f) = f s a

-- | Run the basic printer for the given node without calling an
-- extension hook for this node, but do allow extender hooks in child
-- nodes. Also auto-inserts comments.
prettyNoExt :: Pretty ast => ast NodeInfo -> Printer s ()
prettyNoExt = prettyInternal

-- | Try only the first (i.e. locally best) solution to the given
-- pretty printer.  Use this function to improve performance whenever
-- the formatting of an AST node has no effect on the penalty of any
-- following AST node, such as top-level declarations or case
-- branches.
cut :: Printer s a -> Printer s a
cut p = do
    s <- get
    maybe empty (\(c, (x, s')) -> cost' c >> put s' >> return x) $
        runPrinter p s

withOutputRestriction :: OutputRestriction -> Printer s a -> Printer s a
withOutputRestriction r p = do
    orig <- gets psOutputRestriction
    modify $ \s -> s { psOutputRestriction = max orig r }
    result <- p
    modify $ \s -> s { psOutputRestriction = orig }
    return result

fitsOnOneLineOr :: Printer s a -> Printer s a -> Printer s a
fitsOnOneLineOr single multi = cut $ withOutputRestriction NoOverflowOrLinebreak
                                                           single <|> multi

-- | Print comments of a node.
printComments :: Pretty ast => ComInfoLocation -> ast NodeInfo -> Printer s ()
printComments loc' ast = do
    preprocessor <- gets psCommentPreprocessor

    let correctLocation comment = comInfoLocation comment == Just loc'
        commentsWithLocation = filter correctLocation (nodeInfoComments info)
    comments <- preprocessor $ map comInfoComment commentsWithLocation

    forM_ comments $ \comment -> do
        -- Preceeding comments must have a newline before them.
        hasNewline <- gets psNewline
        when (not hasNewline && loc' == Before) newline

        printComment (Just $ srcInfoSpan $ nodeInfoSpan info) comment
  where
    info = ann ast

-- | Pretty print a comment.
printComment :: Maybe SrcSpan -> Comment -> Printer s ()
printComment mayNodespan (Comment inline cspan str) = do
    -- Insert proper amount of space before comment.
    -- This maintains alignment. This cannot force comments
    -- to go before the left-most possible indent (specified by depends).
    case mayNodespan of
        Just nodespan -> do
            let neededSpaces = srcSpanStartColumn cspan -
                    max 1 (srcSpanEndColumn nodespan)
            replicateM_ neededSpaces space
        Nothing -> return ()

    if inline
        then do
            write "{-"
            string str
            write "-}"
            when (1 == srcSpanStartColumn cspan) $
                modify (\s -> s { psEolComment = True })
        else do
            write "--"
            string str
            modify (\s -> s { psEolComment = True })

-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' :: (Pretty ast, P.Pretty (ast SrcSpanInfo))
        => ast NodeInfo
        -> Printer s ()
pretty' = string . P.prettyPrint . fmap nodeInfoSpan

--------------------------------------------------------------------------------
-- * Combinators
-- | Get the user state.
getState :: Printer s s
getState = gets psUserState

-- | Put the user state.
putState :: s -> Printer s ()
putState s' = modifyState (const s')

-- | Modify the user state.
modifyState :: (s -> s) -> Printer s ()
modifyState f = modify (\s -> s { psUserState = f (psUserState s) })

-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer s a -> Printer s a
indented i p = do
    level <- gets psIndentLevel
    modify (\s -> s { psIndentLevel = level + i })
    m <- p
    modify (\s -> s { psIndentLevel = level })
    return m

indentedBlock :: Printer s a -> Printer s a
indentedBlock p = do
    indentSpaces <- getIndentSpaces
    indented indentSpaces p

-- | Print all the printers separated by spaces.
spaced :: [Printer s ()] -> Printer s ()
spaced = inter space

-- | Print all the printers separated by commas.
commas :: [Printer s ()] -> Printer s ()
commas = inter comma

-- | Print all the printers separated by sep.
inter :: Printer s () -> [Printer s ()] -> Printer s ()
inter sep ps = foldr (\(i, p) next ->
                          depend (do
                                      p
                                      if i < length ps then sep else return ())
                                 next)
                     (return ())
                     (zip [1 ..] ps)

-- | Print all the printers separated by newlines.
lined :: [Printer s ()] -> Printer s ()
lined ps = sequence_ (intersperse newline ps)

-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: S.ByteString -> [Printer s ()] -> Printer s ()
prefixedLined pref ps' = case ps' of
    [] -> return ()
    (p : ps) -> do
        p
        indented (fromIntegral (S.length pref *
                                    (-1)))
                 (mapM_ (\p' -> do
                             newline
                             depend (write pref) p')
                        ps)

-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer s a -> Printer s a
column i p = do
    level <- gets psIndentLevel
    modify (\s -> s { psIndentLevel = i })
    m <- p
    modify (\s -> s { psIndentLevel = level })
    return m

-- | Get the column directly after the last printed character.
getColumn :: Printer s Int64
getColumn = gets psColumn

-- | Get the column for the next printed character.
getNextColumn :: Printer s Int64
getNextColumn = do
    st <- get
    return $
        if psEolComment st
        then psIndentLevel st
        else max (psColumn st) (psIndentLevel st)

-- | Get the current line number.
getLineNum :: Printer s Int64
getLineNum = gets psLine

-- | Output a newline.
newline :: Printer s ()
newline = do
    state <- get
    let clearEmpty = configClearEmptyLines (psConfig state)
        indent = if psNewline state && not clearEmpty
                 then fromIntegral (psIndentLevel state)
                 else 0
        out = S.replicate indent 32
    guard $ psOutputRestriction state /= NoOverflowOrLinebreak
    penalty <- psLinePenalty state True (psColumn state)
    when (penalty /= mempty) $ cost penalty mempty
    modify (\s -> s { psBuffer = Buffer.newline $ Buffer.write out
                                                               (psBuffer state)
                    , psEolComment = False
                    })

-- | Set the context to a case context, where RHS is printed with -> .
withCaseContext :: Bool -> Printer s a -> Printer s a
withCaseContext bool pr = do
    original <- gets psInsideCase
    modify (\s -> s { psInsideCase = bool })
    result <- pr
    modify (\s -> s { psInsideCase = original })
    return result

-- | Get the current RHS separator, either = or -> .
rhsSeparator :: Printer s ()
rhsSeparator = do
    inCase <- gets psInsideCase
    if inCase then write "->" else write "="

-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer s () -> Printer s b -> Printer s b
depend maker dependent = do
    state' <- get
    maker
    st <- get
    col <- gets psColumn
    if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col dependent
        else dependent

-- | Make the latter's indentation depend upon the end column of the
-- former.
dependBind :: Printer s a -> (a -> Printer s b) -> Printer s b
dependBind maker dependent = do
    state' <- get
    v <- maker
    st <- get
    col <- gets psColumn
    if psLine state' /= psLine st || psColumn state' /= psColumn st
        then column col (dependent v)
        else (dependent v)

-- | Wrap in parens.
parens :: Printer s a -> Printer s a
parens p = depend (write "(")
                  (do
                       v <- p
                       write ")"
                       return v)

-- | Wrap in braces.
braces :: Printer s a -> Printer s a
braces p = depend (write "{")
                  (do
                       v <- p
                       write "}"
                       return v)

-- | Wrap in brackets.
brackets :: Printer s a -> Printer s a
brackets p = depend (write "[")
                    (do
                         v <- p
                         write "]"
                         return v)

-- | Write a space.
space :: Printer s ()
space = write " "

-- | Write a comma.
comma :: Printer s ()
comma = write ","

-- | Write an integral.
int :: Integer -> Printer s ()
int = string . show

-- | Write out a string, updating the current position information.
write :: S.ByteString -> Printer s ()
write x = do
    eol <- gets psEolComment
    when eol newline
    write' x
  where
    write' x' = do
        state <- get
        let indent = fromIntegral (psIndentLevel state)
            out = if psNewline state then S.replicate indent 32 <> x' else x'
            buffer = psBuffer state
            newCol = Buffer.column buffer + fromIntegral (S.length out)
        guard $ psOutputRestriction state == Anything || newCol < configMaxColumns (psConfig state)
        penalty <- psLinePenalty state False newCol
        when (penalty /= mempty) $ cost mempty penalty
        modify (\s -> s { psBuffer = Buffer.write out buffer
                        , psEolComment = False
                        })

-- | Write a string.
string :: String -> Printer s ()
string = write . S.toStrict . S.toLazyByteString . S.stringUtf8

-- | Indent spaces, e.g. 2.
getIndentSpaces :: Printer s Int64
getIndentSpaces = gets (configIndentSpaces . psConfig)

-- | Column limit, e.g. 80
getColumnLimit :: Printer s Int64
getColumnLimit = gets (configMaxColumns . psConfig)

-- | Play with a printer and then restore the state to what it was
-- before.
sandbox :: MonadState s m => m a -> m (a, s)
sandbox p = do
    orig <- get
    a <- p
    new <- get
    put orig
    return (a, new)

-- | No binds?
nullBinds :: Binds NodeInfo -> Bool
nullBinds (BDecls _ x) = null x
nullBinds (IPBinds _ x) = null x

-- | Maybe render a class context.
maybeCtx :: Maybe (Context NodeInfo) -> Printer s ()
maybeCtx = maybe (return ())
                 (\p -> pretty p >>
                      write " => ")

-- | Render a type with a context, or not.
withCtx :: Pretty ast => Maybe (ast NodeInfo) -> Printer s b -> Printer s b
withCtx Nothing m = m
withCtx (Just ctx) m = do
    pretty ctx
    write " =>"
    newline
    m

-- | Maybe render an overlap definition.
maybeOverlap :: Maybe (Overlap NodeInfo) -> Printer s ()
maybeOverlap = maybe (return ())
                     (\p -> pretty p >>
                          space)

-- | Swing the second printer below and indented with respect to the first.
swing :: Printer s () -> Printer s b -> Printer s b
swing a b = do
    orig <- gets psIndentLevel
    a
    newline
    indentSpaces <- getIndentSpaces
    column (orig + indentSpaces) b

-- | Swing the second printer below and indented with respect to the first by
-- the specified amount.
swingBy :: Int64 -> Printer s () -> Printer s b -> Printer s b
swingBy i a b = do
    orig <- gets psIndentLevel
    a
    newline
    column (orig + i) b

--------------------------------------------------------------------------------
-- * Instances
instance Pretty Context where
    prettyInternal ctx = case ctx of
        CxSingle _ a -> pretty a
        CxTuple _ as -> parens (prefixedLined "," (map pretty as))
        CxEmpty _ -> parens (return ())

instance Pretty Pat where
    prettyInternal x = case x of
        PLit _ sign l -> pretty sign >> pretty l
        PNPlusK _ n k -> depend (do
                                     pretty n
                                     write "+")
                                (int k)
        PInfixApp _ a op b -> case op of
            Special{} -> depend (pretty a)
                                (depend (prettyInfixOp op) (pretty b))
            _ -> depend (do
                             pretty a
                             space)
                        (depend (do
                                     prettyInfixOp op
                                     space)
                                (pretty b))
        PApp _ f args -> depend (do
                                     pretty f
                                     unless (null args) space)
                                (spaced (map pretty args))
        PTuple _ boxed pats -> depend (write (case boxed of
                                                  Unboxed -> "(# "
                                                  Boxed -> "("))
                                      (do
                                           commas (map pretty pats)
                                           write (case boxed of
                                                      Unboxed -> " #)"
                                                      Boxed -> ")"))
        PList _ ps -> brackets (commas (map pretty ps))
        PParen _ e -> parens (pretty e)
        PRec _ qname fields -> do
            indentSpaces <- getIndentSpaces
            depend (do
                        pretty qname
                        space)
                   (braces (prefixedLined ","
                                          (map (indented indentSpaces . pretty)
                                               fields)))

        PAsPat _ n p -> depend (do
                                    pretty n
                                    write "@")
                               (pretty p)
        PWildCard _ -> write "_"
        PIrrPat _ p -> depend (write "~") (pretty p)
        PatTypeSig _ p ty -> depend (do
                                         pretty p
                                         write " :: ")
                                    (pretty ty)
        PViewPat _ e p -> depend (do
                                      pretty e
                                      write " -> ")
                                 (pretty p)
        PQuasiQuote _ name str -> brackets (depend (do
                                                        write "$"
                                                        string name
                                                        write "|")
                                                   (string str))
        PBangPat _ p -> depend (write "!") (pretty p)
        PRPat{} -> pretty' x
        PXTag{} -> pretty' x
        PXETag{} -> pretty' x
        PXPcdata{} -> pretty' x
        PXPatTag{} -> pretty' x
        PXRPats{} -> pretty' x
        PVar{} -> pretty' x

-- | Pretty print a name for being an infix operator.
prettyInfixOp :: QName NodeInfo -> Printer s ()
prettyInfixOp op = case op of
    Qual _ mn n -> case n of
        Ident _ i -> do
            write "`"
            pretty mn
            write "."
            string i
            write "`"
        Symbol _ s -> do
            pretty mn
            write "."
            string s
    UnQual _ n -> case n of
        Ident _ i -> string ("`" ++ i ++ "`")
        Symbol _ s -> string s
    Special _ s -> pretty s

instance Pretty Type where
    prettyInternal x = case x of
        TyForall _ mbinds ctx ty -> depend (case mbinds of
                                                Nothing -> return ()
                                                Just ts -> do
                                                    write "forall "
                                                    spaced (map pretty ts)
                                                    write ". ")
                                           (withCtx ctx (pretty ty))
        TyFun _ a b -> depend (do
                                   pretty a
                                   write " -> ")
                              (pretty b)
        TyTuple _ boxed tys -> depend (write (case boxed of
                                                  Unboxed -> "(# "
                                                  Boxed -> "("))
                                      (do
                                           commas (map pretty tys)
                                           write (case boxed of
                                                      Unboxed -> " #)"
                                                      Boxed -> ")"))
        TyList _ t -> brackets (pretty t)
        TyParArray _ t -> brackets (do
                                        write ":"
                                        pretty t
                                        write ":")
        TyApp _ f a -> spaced [ pretty f, pretty a ]
        TyVar _ n -> pretty n
        TyCon _ p -> pretty p
        TyParen _ e -> parens (pretty e)
        TyInfix _ a op b -> depend (do
                                        pretty a
                                        space)
                                   (depend (do
                                                prettyInfixOp op
                                                space)
                                           (pretty b))
        TyKind _ ty k -> parens (do
                                     pretty ty
                                     write " :: "
                                     pretty k)
        TyBang _ bangty unpackty right -> do
            pretty unpackty
            pretty bangty
            pretty right
        TyEquals _ left right -> do
            pretty left
            write " ~ "
            pretty right
        ty@TyPromoted{} -> pretty' ty
        TySplice _ splice -> pretty splice
        TyWildCard _ name -> case name of
            Nothing -> write "_"
            Just n -> do
                write "_"
                pretty n
        TyQuasiQuote _ n s -> brackets (depend (do
                                                    string n
                                                    write "|")
                                               (do
                                                    string s
                                                    write "|"))

instance Pretty Exp where
    prettyInternal = exp

-- | Render an expression.
exp :: Exp NodeInfo -> Printer s ()
exp (ExprHole{}) = write "_"
exp (InfixApp _ a op b) = depend (do
                                      pretty a
                                      space
                                      pretty op
                                      space)
                                 (do
                                      pretty b)
exp (App _ op a) = swing (do
                              pretty f)
                         (lined (map pretty args))
  where
    (f, args) = flatten op [ a ]
    flatten :: Exp NodeInfo -> [Exp NodeInfo] -> (Exp NodeInfo, [Exp NodeInfo])
    flatten (App _ f' a') b = flatten f' (a' : b)
    flatten f' as = (f', as)
exp (NegApp _ e) = depend (write "-") (pretty e)
exp (Lambda _ ps e) = depend (write "\\" >> maybeSpace)
                             (do
                                  spaced (map pretty ps)
                                  swing (write " -> ") (pretty e))
  where
    maybeSpace = case ps of
        (PBangPat{}) : _ -> space
        (PIrrPat{}) : _ -> space
        _ -> return ()
exp (Let _ binds e) = do
    depend (write "let ") (pretty binds)
    newline
    depend (write "in ") (pretty e)
exp (If _ p t e) = do
    depend (write "if ")
           (do
                pretty p
                newline
                depend (write "then ") (pretty t)
                newline
                depend (write "else ") (pretty e))
exp (Paren _ e) = parens (pretty e)
exp (Case _ e alts) = do
    depend (write "case ")
           (do
                pretty e
                write " of")
    if null alts
        then write " {}"
        else do
            newline
            indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (Do _ stmts) = depend (write "do ") (lined (map pretty stmts))
exp (MDo _ stmts) = depend (write "mdo ") (lined (map pretty stmts))
exp (Tuple _ boxed exps) = depend (write (case boxed of
                                              Unboxed -> "(# "
                                              Boxed -> "("))
                                  (do
                                       prefixedLined "," (map pretty exps)
                                       write (case boxed of
                                                  Unboxed -> " #)"
                                                  Boxed -> ")"))
exp (TupleSection _ boxed mexps) =
    depend (write (case boxed of
                       Unboxed -> "(# "
                       Boxed -> "("))
           (do
                commas (map (maybe (return ()) pretty) mexps)
                write (case boxed of
                           Unboxed -> " #)"
                           Boxed -> ")"))
exp (List _ es) = brackets (prefixedLined "," (map pretty es))
exp (LeftSection _ e op) = parens (depend (do
                                               pretty e
                                               space)
                                          (pretty op))
exp (RightSection _ e op) = parens (depend (do
                                                pretty e
                                                space)
                                           (pretty op))
exp (RecConstr _ n fs) =
    depend (do
                pretty n
                space)
           (braces (prefixedLined "," (map (indentedBlock . pretty) fs)))
exp (RecUpdate _ n fs) =
    depend (do
                pretty n
                space)
           (braces (prefixedLined "," (map (indentedBlock . pretty) fs)))
exp (EnumFrom _ e) = brackets (do
                                   pretty e
                                   write " ..")
exp (EnumFromTo _ e f) = brackets (depend (do
                                               pretty e
                                               write " .. ")
                                          (pretty f))
exp (EnumFromThen _ e t) = brackets (depend (do
                                                 pretty e
                                                 write ",")
                                            (do
                                                 pretty t
                                                 write " .."))
exp (EnumFromThenTo _ e t f) = brackets (depend (do
                                                     pretty e
                                                     write ",")
                                                (depend (do
                                                             pretty t
                                                             write " .. ")
                                                        (pretty f)))
exp (ListComp _ e qstmt) =
    brackets (do
                  pretty e
                  unless (null qstmt)
                         (do
                              newline
                              indented (-1) (write "|")
                              prefixedLined "," (map pretty qstmt)))
exp (ExpTypeSig _ e t) = depend (do
                                     pretty e
                                     write " :: ")
                                (pretty t)
exp (VarQuote _ x) = depend (write "'") (pretty x)
exp (TypQuote _ x) = depend (write "''") (pretty x)
exp (BracketExp _ b) = pretty b
exp (SpliceExp _ s) = pretty s
exp (QuasiQuote _ n s) = brackets (depend (do
                                               string n
                                               write "|")
                                          (do
                                               string s
                                               write "|"))
exp (LCase _ alts) = do
    write "\\case"
    if null alts
        then write " {}"
        else do
            newline
            indentedBlock (lined (map (withCaseContext True . pretty) alts))
exp (MultiIf _ alts) = withCaseContext True
                                       (depend (write "if ")
                                               (lined (map (\p -> do
                                                                write "| "
                                                                prettyG p)
                                                           alts)))
  where
    prettyG (GuardedRhs _ stmts e) = do
        indented 1
                 (do
                      (lined (map (\(i, p) -> do
                                       unless (i == 1) space
                                       pretty p
                                       unless (i == length stmts) (write ","))
                                  (zip [1 ..] stmts))))
        swing (write " " >> rhsSeparator) (pretty e)

exp (Lit _ lit) = prettyInternal lit
exp (TypeApp _ t) = do
    write "@"
    pretty t
exp x@XTag{} = pretty' x
exp x@XETag{} = pretty' x
exp x@XPcdata{} = pretty' x
exp x@XExpTag{} = pretty' x
exp x@XChildTag{} = pretty' x
exp x@Var{} = pretty' x
exp x@IPVar{} = pretty' x
exp x@Con{} = pretty' x
exp x@CorePragma{} = pretty' x
exp x@SCCPragma{} = pretty' x
exp x@GenPragma{} = pretty' x
exp x@Proc{} = pretty' x
exp x@LeftArrApp{} = pretty' x
exp x@RightArrApp{} = pretty' x
exp x@LeftArrHighApp{} = pretty' x
exp x@RightArrHighApp{} = pretty' x
exp x@ParArray{} = pretty' x
exp x@ParArrayFromTo{} = pretty' x
exp x@ParArrayFromThenTo{} = pretty' x
exp x@ParArrayComp{} = pretty' x
exp x@ParComp{} = pretty' x
exp (OverloadedLabel _ label) = string ('#' : label)

instance Pretty IPName where
    prettyInternal = pretty'

instance Pretty Stmt where
    prettyInternal x = case x of
        Generator _ p e -> depend (do
                                       pretty p
                                       write " <- ")
                                  (pretty e)
        Qualifier _ e -> pretty e
        LetStmt _ binds -> depend (write "let ") (pretty binds)
        RecStmt{} -> error "FIXME: No implementation for RecStmt."

instance Pretty QualStmt where
    prettyInternal x = case x of
        QualStmt _ s -> pretty s
        ThenTrans _ s -> do
            write "then "
            pretty s
        ThenBy _ s t -> do
            write "then "
            pretty s
            write " by "
            pretty t
        GroupBy _ s -> do
            write "then group by "
            pretty s
        GroupUsing _ s -> do
            write "then group using "
            pretty s
        GroupByUsing _ s t -> do
            write "then group by "
            pretty s
            write " using "
            pretty t

instance Pretty Decl where
    prettyInternal = decl

-- | Render a declaration.
decl :: Decl NodeInfo -> Printer s ()
decl (PatBind _ pat rhs mbinds) = do
    pretty pat
    withCaseContext False (pretty rhs)
    case mbinds of
        Nothing -> return ()
        Just binds -> do
            newline
            indentedBlock (depend (write "where ") (pretty binds))
decl (InstDecl _ moverlap dhead decls) = do
    depend (write "instance ")
           (depend (maybeOverlap moverlap)
                   (depend (pretty dhead)
                           (unless (null (fromMaybe [] decls)) (write " where"))))
    unless (null (fromMaybe [] decls))
           (do
                newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
decl (SpliceDecl _ e) = pretty e
decl (TypeSig _ names ty) = depend (do
                                        inter (write ", ") (map pretty names)
                                        write " :: ")
                                   (pretty ty)
decl (FunBind _ matches) = lined (map pretty matches)
decl (ClassDecl _ ctx dhead fundeps decls) = do
    depend (write "class ")
           (withCtx ctx
                    (depend (do
                                 pretty dhead)
                            (depend (unless (null fundeps)
                                            (do
                                                 write " | "
                                                 commas (map pretty fundeps)))
                                    (unless (null (fromMaybe [] decls))
                                            (write " where")))))
    unless (null (fromMaybe [] decls))
           (do
                newline
                indentedBlock (lined (map pretty (fromMaybe [] decls))))
decl (TypeDecl _ typehead typ) =
    depend (write "type ")
           (depend (pretty typehead) (depend (write " = ") (pretty typ)))

decl (TypeFamDecl _ declhead result injectivity) = do
    write "type family "
    pretty declhead
    case result of
        Just r -> do
            space
            write "="
            space
            pretty r
        Nothing -> return ()
    case injectivity of
        Just i -> do
            space
            pretty i
        Nothing -> return ()
decl (DataDecl _ dataornew ctx dhead condecls mderivs) = do
    depend (do
                pretty dataornew
                space)
           (withCtx ctx
                    (do
                         pretty dhead
                         case condecls of
                             [] -> return ()
                             [ x ] -> singleCons x
                             xs -> multiCons xs))
    indentSpaces <- getIndentSpaces
    case mderivs of
        Nothing -> return ()
        Just derivs -> do
            newline
            column indentSpaces (pretty derivs)
  where
    singleCons x = do
        write " ="
        indentSpaces <- getIndentSpaces
        column indentSpaces
               (do
                    newline
                    pretty x)
    multiCons xs = do
        newline
        indentSpaces <- getIndentSpaces
        column indentSpaces
               (depend (write "=")
                       (prefixedLined "|" (map (depend space . pretty) xs)))

decl (InlineSig _ inline _ name) = do
    write "{-# "

    unless inline $ write "NO"
    write "INLINE "
    pretty name

    write " #-}"
decl x = pretty' x

instance Pretty Deriving where
    prettyInternal (Deriving _ heads) = do
        write "deriving"
        space
        let heads' = if length heads == 1 then map stripParens heads else heads
        parens (commas (map pretty heads'))
      where
        stripParens (IParen _ iRule) = stripParens iRule
        stripParens x = x

instance Pretty Alt where
    prettyInternal x = case x of
        Alt _ p galts mbinds -> do
            pretty p
            pretty galts
            case mbinds of
                Nothing -> return ()
                Just binds -> do
                    newline
                    indentedBlock (depend (write "where ") (pretty binds))

instance Pretty Asst where
    prettyInternal x = case x of
        ClassA _ name types -> spaced (pretty name : map pretty types)
        i@InfixA{} -> pretty' i
        IParam _ name ty -> do
            pretty name
            write " :: "
            pretty ty
        EqualP _ a b -> do
            pretty a
            write " ~ "
            pretty b
        ParenA _ asst -> parens (pretty asst)
        AppA _ name tys -> spaced (pretty name : map pretty tys)
        WildCardA _ name -> case name of
            Nothing -> write "_"
            Just n -> do
                write "_"
                pretty n

instance Pretty BangType where
    prettyInternal x = case x of
        BangedTy _ -> write "!"
        LazyTy _ -> write "~"
        NoStrictAnnot _ -> return ()

instance Pretty Unpackedness where
    prettyInternal (Unpack _) = write "{-# UNPACK -#}"
    prettyInternal (NoUnpack _) = write "{-# NOUNPACK -#}"
    prettyInternal (NoUnpackPragma _) = return ()

instance Pretty Binds where
    prettyInternal x = case x of
        BDecls _ ds -> lined (map pretty ds)
        IPBinds _ i -> lined (map pretty i)

instance Pretty ClassDecl where
    prettyInternal x = case x of
        ClsDecl _ d -> pretty d
        ClsDataFam _ ctx h mkind ->
            depend (write "data ")
                   (withCtx ctx
                            (do
                                 pretty h
                                 (case mkind of
                                      Nothing -> return ()
                                      Just kind -> do
                                          write " :: "
                                          pretty kind)))
        ClsTyFam _ h mkind minj ->
            depend (write "type ")
                   (depend (pretty h)
                           (depend (traverse_ (\kind ->
                                                   write " :: " >> pretty kind)
                                              mkind)
                                   (traverse_ pretty minj)))
        ClsTyDef _ (TypeEqn _ this that) ->
            depend (write "type ")
                   (depend (pretty this) (depend (write " = ") (pretty that)))
        ClsDefSig _ name ty ->
            depend (write "default ")
                   (depend (pretty name) (depend (write " :: ") (pretty ty)))

instance Pretty ConDecl where
    prettyInternal x = case x of
        ConDecl _ name [] -> pretty name
        ConDecl _ name bangty -> depend (do
                                             pretty name
                                             space)
                                        (lined (map pretty bangty))
        InfixConDecl l a f b -> pretty (ConDecl l f [ a, b ])
        RecDecl _ name fields ->
            depend (do
                        pretty name
                        write " ")
                   (do
                        depend (write "{")
                               (prefixedLined "," (map pretty fields))
                        write "}")

instance Pretty FieldDecl where
    prettyInternal (FieldDecl _ names ty) = depend (do
                                                        commas (map pretty
                                                                    names)
                                                        write " :: ")
                                                   (pretty ty)

instance Pretty FieldUpdate where
    prettyInternal x = case x of
        FieldUpdate _ n e -> swing (do
                                        pretty n
                                        write " = ")
                                   (pretty e)
        FieldPun _ n -> pretty n
        FieldWildcard _ -> write ".."

instance Pretty GuardedRhs where
    prettyInternal x = case x of
        GuardedRhs _ stmts e -> do
            indented 1
                     (do
                          prefixedLined ","
                                        (map (\p -> do
                                                  space
                                                  pretty p)
                                             stmts))
            swing (write " " >> rhsSeparator >> write " ") (pretty e)

instance Pretty InjectivityInfo where
    prettyInternal x = pretty' x

instance Pretty InstDecl where
    prettyInternal i = case i of
        InsDecl _ d -> pretty d
        InsType _ name ty -> depend (do
                                         write "type "
                                         pretty name
                                         write " = ")
                                    (pretty ty)
        _ -> pretty' i

instance Pretty Match where
    prettyInternal x = case x of
        Match _ name pats rhs mbinds -> do
            depend (do
                        pretty name
                        space)
                   (spaced (map pretty pats))
            withCaseContext False (pretty rhs)
            case mbinds of
                Nothing -> return ()
                Just binds -> do
                    newline
                    indentedBlock (depend (write "where ") (pretty binds))
        InfixMatch _ pat1 name pats rhs mbinds -> do
            depend (do
                        pretty pat1
                        space
                        case name of
                            Ident _ i -> string ("`" ++ i ++ "`")
                            Symbol _ s -> string s)
                   (do
                        space
                        spaced (map pretty pats))
            withCaseContext False (pretty rhs)
            case mbinds of
                Nothing -> return ()
                Just binds -> do
                    newline
                    indentedBlock (depend (write "where ") (pretty binds))

instance Pretty PatField where
    prettyInternal x = case x of
        PFieldPat _ n p -> depend (do
                                       pretty n
                                       write " = ")
                                  (pretty p)
        PFieldPun _ n -> pretty n
        PFieldWildcard _ -> write ".."

instance Pretty QualConDecl where
    prettyInternal x = case x of
        QualConDecl _ tyvars ctx d ->
            depend (unless (null (fromMaybe [] tyvars))
                           (do
                                write "forall "
                                spaced (map pretty (fromMaybe [] tyvars))
                                write ". "))
                   (withCtx ctx (pretty d))

instance Pretty Rhs where
    prettyInternal x = case x of
        UnGuardedRhs _ e -> do
            (swing (write " " >> rhsSeparator >> write " ") (pretty e))
        GuardedRhss _ gas -> do
            newline
            indented 2
                     (lined (map (\p -> do
                                      write "|"
                                      pretty p)
                                 gas))

instance Pretty Splice where
    prettyInternal x = case x of
        IdSplice _ str -> do
            write "$"
            string str
        ParenSplice _ e -> depend (write "$") (parens (pretty e))

instance Pretty InstRule where
    prettyInternal (IParen _ rule) = parens $ pretty rule
    prettyInternal (IRule _ mvarbinds mctx ihead) = do
        case mvarbinds of
            Nothing -> return ()
            Just xs -> spaced (map pretty xs)
        withCtx mctx (pretty ihead)

instance Pretty InstHead where
    prettyInternal x = case x of
        -- Base cases
        IHCon _ name -> pretty name
        IHInfix _ typ name -> depend (pretty typ)
                                     (do
                                          space
                                          prettyInfixOp name)
        -- Recursive application
        IHApp _ ihead typ -> depend (pretty ihead)
                                    (do
                                         space
                                         pretty typ)
        -- Wrapping in parens
        IHParen _ h -> parens (pretty h)

instance Pretty DeclHead where
    prettyInternal x = case x of
        DHead _ name -> pretty name
        DHParen _ h -> parens (pretty h)
        DHInfix _ var name -> do
            pretty var
            space
            write "`"
            pretty name
            write "`"
        DHApp _ dhead var -> depend (pretty dhead)
                                    (do
                                         space
                                         pretty var)

instance Pretty SpecialCon where
    prettyInternal s = case s of
        UnitCon _ -> write "()"
        ListCon _ -> write "[]"
        FunCon _ -> write "->"
        TupleCon _ Boxed i -> string ("(" ++
                                          replicate (i - 1) ',' ++
                                              ")")
        TupleCon _ Unboxed i -> string ("(# " ++
                                            replicate (i - 1) ',' ++
                                                " #)")
        Cons _ -> write ":"
        UnboxedSingleCon _ -> write "(##)"

instance Pretty Overlap where
    prettyInternal (Overlap _) = write "{-# OVERLAP #-}"
    prettyInternal (NoOverlap _) = write "{-# NO_OVERLAP #-}"
    prettyInternal (Incoherent _) = write "{-# INCOHERENT #-}"

instance Pretty Sign where
    prettyInternal (Signless _) = return ()
    prettyInternal (Negative _) = write "-"

--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers
instance Pretty Module where
    prettyInternal x = case x of
        Module _ mayModHead pragmas imps decls ->
            inter (do
                       newline
                       newline)
                  (mapMaybe (\(isNull, r) -> if isNull then Nothing else Just r)
                            [ ( null pragmas
                              , inter newline (map pretty pragmas)
                              )
                            , (case mayModHead of
                                   Nothing -> (True, return ())
                                   Just modHead -> (False, pretty modHead))
                            , (null imps, inter newline (map pretty imps))
                            , ( null decls
                              , interOf newline
                                        (map (\case
                                                  r@TypeSig{} -> (1, pretty r)
                                                  r -> (2, pretty r))
                                             decls)
                              )
                            ])
            where interOf i ((c, p) : ps) = case ps of
                      [] -> p
                      _ -> do
                          p
                          replicateM_ c i
                          interOf i ps
                  interOf _ [] = return ()
        XmlPage{} -> error "FIXME: No implementation for XmlPage."
        XmlHybrid{} -> error "FIXME: No implementation for XmlHybrid."

instance Pretty Bracket where
    prettyInternal x = case x of
        ExpBracket _ p -> brackets (depend (write "|")
                                           (do
                                                pretty p
                                                write "|"))
        PatBracket _ p -> brackets (depend (write "p|")
                                           (do
                                                pretty p
                                                write "|"))
        TypeBracket _ ty -> brackets (depend (write "t|")
                                             (do
                                                  pretty ty
                                                  write "|"))
        d@(DeclBracket _ _) -> pretty' d

instance Pretty IPBind where
    prettyInternal x = case x of
        IPBind _ name expr -> do
            pretty name
            space
            write "="
            space
            pretty expr

--------------------------------------------------------------------------------
-- * Fallback printers
instance Pretty DataOrNew where
    prettyInternal = pretty'

instance Pretty FunDep where
    prettyInternal = pretty'

instance Pretty Kind where
    prettyInternal = pretty'

instance Pretty ResultSig where
    prettyInternal (KindSig _ kind) = pretty kind
    prettyInternal (TyVarSig _ tyVarBind) = pretty tyVarBind

instance Pretty Literal where
    prettyInternal (String _ _ rep) = do
        write "\""
        string rep
        write "\""
    prettyInternal (Char _ _ rep) = do
        write "'"
        string rep
        write "'"
    prettyInternal (PrimString _ _ rep) = do
        write "\""
        string rep
        write "\"#"
    prettyInternal (PrimChar _ _ rep) = do
        write "'"
        string rep
        write "'#"
    -- We print the original notation (because HSE doesn't track Hex
    -- vs binary vs decimal notation).
    prettyInternal (Int _l _i originalString) = string originalString
    prettyInternal (Frac _l _r originalString) = string originalString
    prettyInternal x = pretty' x

instance Pretty Name where
    prettyInternal = pretty'

instance Pretty QName where
    prettyInternal = pretty'

instance Pretty QOp where
    prettyInternal = pretty'

instance Pretty TyVarBind where
    prettyInternal = pretty'

instance Pretty ModuleHead where
    prettyInternal (ModuleHead _ name mwarnings mexports) = do
        write "module "
        pretty name
        maybe (return ()) pretty mwarnings
        maybe (return ())
              (\exports -> do
                   newline
                   indented 2 (pretty exports)
                   newline
                   space)
              mexports
        write " where"

instance Pretty ModulePragma where
    prettyInternal = pretty'

instance Pretty ImportDecl where
    prettyInternal = pretty'

instance Pretty ModuleName where
    prettyInternal (ModuleName _ name) = string name

instance Pretty ImportSpecList where
    prettyInternal = pretty'

instance Pretty ImportSpec where
    prettyInternal = pretty'

instance Pretty WarningText where
    prettyInternal (DeprText _ s) = write "{-# DEPRECATED " >> string s >> write " #-}"
    prettyInternal (WarnText _ s) = write "{-# WARNING " >> string s >> write " #-}"

instance Pretty ExportSpecList where
    prettyInternal (ExportSpecList _ es) = parens (prefixedLined ","
                                                                 (map pretty es))

instance Pretty ExportSpec where
    prettyInternal = pretty'

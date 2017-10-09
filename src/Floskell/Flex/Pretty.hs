{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Floskell.Flex.Pretty where

import           Control.Applicative          ( (<|>) )
import           Control.Monad                ( forM_, unless, void, when )
import           Control.Monad.State.Strict   ( gets )

import           Data.ByteString              ( ByteString )
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8

import           Data.List                    ( groupBy, sortBy, sortOn )
import           Data.Maybe                   ( catMaybes )

import           Floskell.Flex.Config
import           Floskell.Flex.Printers

import           Floskell.Pretty              ( column, getNextColumn
                                              , printComment, printComments )
import           Floskell.Types

import qualified Language.Haskell.Exts.Pretty as HSE
import           Language.Haskell.Exts.SrcLoc ( noSrcSpan, srcInfoSpan )
import           Language.Haskell.Exts.Syntax

-- | Like `span`, but comparing adjacent items.
run :: (a -> a -> Bool) -> [a] -> ([a], [a])
run _ [] = ([], [])
run _ [ x ] = ([ x ], [])
run eq (x : y : xs)
    | eq x y = let (ys, zs) = run eq (y : xs)
               in
                   (x : ys, zs)
    | otherwise = ([ x ], y : xs)

-- | Like `groupBy`, but comparing adjacent items.
runs :: (a -> a -> Bool) -> [a] -> [[a]]
runs _ [] = []
runs eq xs = let (ys, zs) = run eq xs
             in
                 ys : runs eq zs

flattenl :: (a -> Maybe (a, b)) -> a -> (a, [b])
flattenl fn x = case fn x of
    Just (lhs, rhs) -> let (first, rest) = flattenl fn lhs in (first, rest ++ [rhs])
    Nothing -> (x, [])

flattenr :: (a -> Maybe (b, a)) -> a -> ([b], a)
flattenr fn x = case fn x of
    Just (lhs, rhs) -> let (rest, lst) = flattenr fn rhs in (lhs : rest, lst)
    Nothing -> ([], x)

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

-- | Empty NodeInfo
noNodeInfo :: NodeInfo
noNodeInfo = NodeInfo noSrcSpan []

-- | Compare two AST nodes ignoring the annotation
compareAST :: (Functor ast, Ord (ast ()))
           => ast NodeInfo
           -> ast NodeInfo
           -> Ordering
compareAST a b = void a `compare` void b

lined :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer FlexConfig ()
lined = inter newline . map (cut . pretty)

listVinternal :: (Annotated ast, Pretty ast)
              => ByteString
              -> [ast NodeInfo]
              -> Printer FlexConfig ()
listVinternal sep xs = aligned $ do
    ws <- getConfig (cfgOpWs sep . cfgOp)
    nl <- gets psNewline
    col <- getNextColumn
    let correction = if wsLinebreak After ws
                     then 0
                     else BS.length sep + if wsSpace After ws then 1 else 0
        extraIndent = if nl then correction else 0
        itemCol = col + fromIntegral extraIndent
        sepCol = itemCol - fromIntegral correction
    case xs of
        [] -> newline
        (x : xs') -> column itemCol $ do
            cut $ do
                printCommentsSimple Before x
                prettyPrint x
                printCommentsSimple After x
            forM_ xs' $ \x' -> do
                printComments Before x'
                column sepCol $ operatorV sep
                cut $ prettyPrint x'
                printComments After x'
  where
    printCommentsSimple loc ast = let comments = map comInfoComment .
                                          filter ((== Just loc) . comInfoLocation) .
                                              nodeInfoComments $ ann ast
                                  in
                                      forM_ comments $ \comment -> do
                                          printComment (Just $ srcInfoSpan $
                                                            nodeInfoSpan $ ann ast)
                                                       comment

listH :: (Annotated ast, Pretty ast)
      => ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
listH open close _ [] = do
    write open
    write close

listH open close sep xs =
    groupH open close . inter (operatorH sep) $ map pretty xs

listV :: (Annotated ast, Pretty ast)
      => ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
listV open close sep xs = groupV open close $ do
    ws <- getConfig (cfgOpWs sep . cfgOp)
    ws' <- getConfig (cfgGroupWs open . cfgGroup)
    unless ((wsLinebreak Before ws')
            || (wsSpace After ws')
            || (wsLinebreak After ws)
            || not (wsSpace After ws))
           space
    listVinternal sep xs

list :: (Annotated ast, Pretty ast)
     => ByteString
     -> ByteString
     -> ByteString
     -> [ast NodeInfo]
     -> Printer FlexConfig ()
list open close sep xs = oneline hor <|> ver
  where
    hor = listH open close sep xs
    ver = listV open close sep xs

listH' :: (Annotated ast, Pretty ast)
       => ByteString
       -> ByteString
       -> [ast NodeInfo]
       -> Printer FlexConfig ()
listH' op sep xs = do
    operator op
    inter (operatorH sep) $ map pretty xs

listV' :: (Annotated ast, Pretty ast)
       => ByteString
       -> ByteString
       -> [ast NodeInfo]
       -> Printer FlexConfig ()
listV' op sep xs = do
    operator op
    listVinternal sep xs

list' :: (Annotated ast, Pretty ast)
      => ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
list' op sep xs = oneline hor <|> ver
  where
    hor = listH' op sep xs
    ver = listV' op sep xs

listAutoWrap :: (Annotated ast, Pretty ast)
             => ByteString
             -> ByteString
             -> ByteString
             -> [ast NodeInfo]
             -> Printer FlexConfig ()
listAutoWrap open close _ [] = do
    write open
    write close

listAutoWrap open close sep (x : xs) = groupH open close . aligned $ do
    ws <- getConfig (cfgOpWs sep . cfgOp)
    let correction = if wsLinebreak After ws
                     then 0
                     else BS.length sep + if wsSpace After ws then 1 else 0
    col <- getNextColumn
    pretty x
    forM_ xs $ \x' -> do
        printComments Before x'
        cut $ do
            column (col - fromIntegral correction) $ operator sep
            prettyPrint x'
            printComments After x'

------------------------------------------------------------------------
-- Module
-- | Extract the name as a String from a ModuleName
moduleName :: ModuleName a -> String
moduleName (ModuleName _ s) = s

prettyPragmas :: [ModulePragma NodeInfo] -> Printer FlexConfig ()
prettyPragmas ps = do
    splitP <- getConfig (cfgModuleSplitLanguagePragmas . cfgModule)
    sortP <- getConfig (cfgModuleSortPragmas . cfgModule)
    let ps' = if splitP then concatMap splitPragma ps else ps
    let ps'' = if sortP then sortBy compareAST ps' else ps'
    lined ps''
  where
    splitPragma (LanguagePragma anno langs) = map (LanguagePragma anno . (: []))
                                                  langs
    splitPragma p = [ p ]

prettyImports :: [ImportDecl NodeInfo] -> Printer FlexConfig ()
prettyImports is = do
    sortP <- getConfig (cfgModuleSortImports . cfgModule)
    if sortP
        then inter blankline . map lined .
            groupBy samePrefix $ sortOn (moduleName . importModule) is
        else lined is
  where
    samePrefix left right = prefix left == prefix right
    prefix = takeWhile (/= '.') . moduleName . importModule

skipBlankDecl :: Decl a -> Bool
skipBlankDecl TypeSig{} = True
skipBlankDecl DeprPragmaDecl{} = True
skipBlankDecl WarnPragmaDecl{} = True
skipBlankDecl AnnPragma{} = True
skipBlankDecl MinimalPragma{} = True
skipBlankDecl InlineSig{} = True
skipBlankDecl InlineConlikeSig{} = True
skipBlankDecl SpecSig{} = True
skipBlankDecl SpecInlineSig{} = True
skipBlankDecl InstSig{} = True
skipBlankDecl PatSynSig{} = True
skipBlankDecl _ = False

skipBlankClassDecl :: ClassDecl a -> Bool
skipBlankClassDecl (ClsDecl _ decl) = skipBlankDecl decl
skipBlankClassDecl ClsTyDef{} = True
skipBlankClassDecl ClsDefSig{} = True
skipBlankClassDecl _ = False

skipBlankInstDecl :: InstDecl a -> Bool
skipBlankInstDecl (InsDecl _ decl) = skipBlankDecl decl
skipBlankInstDecl _ = False

prettyDecls :: (Annotated ast, Pretty ast)
            => (ast NodeInfo -> ast NodeInfo -> Bool)
            -> [ast NodeInfo]
            -> Printer FlexConfig ()
prettyDecls fn = inter blankline . map lined . runs fn

prettyForall :: (Annotated ast, Pretty ast)
             => [ast NodeInfo]
             -> Printer FlexConfig ()
prettyForall vars = do
    write "forall "
    inter space $ map pretty vars
    write "."
    sepSpace

prettyPragma :: ByteString -> Printer FlexConfig () -> Printer FlexConfig ()
prettyPragma name = prettyPragma' name . Just

prettyPragma' :: ByteString
              -> Maybe (Printer FlexConfig ())
              -> Printer FlexConfig ()
prettyPragma' name mp = do
    write "{-# "
    write name
    mayM_ mp $ withPrefix space aligned
    write " #-}"

instance Pretty Module where
    prettyPrint (Module _ mhead pragmas imports decls) = inter blankline $
        catMaybes [ ifNotEmpty prettyPragmas pragmas
                  , pretty <$> mhead
                  , ifNotEmpty prettyImports imports
                  , ifNotEmpty (prettyDecls (\d _ -> skipBlankDecl d)) decls
                  ]
      where
        ifNotEmpty f xs = if null xs then Nothing else Just (f xs)

    prettyPrint ast@XmlPage{} = prettyHSE ast
    prettyPrint ast@XmlHybrid{} = prettyHSE ast

instance Pretty ModuleHead where
    prettyPrint (ModuleHead _ name mwarning mexports) = depend "module" $ do
        pretty name
        mayM_ mwarning $ withPrefix spaceOrNewline pretty
        mayM_ mexports $ withPrefix spaceOrNewline pretty
        write " where"

instance Pretty WarningText where
    prettyPrint (DeprText _ s) = write "{-# DEPRECATED " >> string (show s) >> write " #-}"
    prettyPrint (WarnText _ s) = write "{-# WARNING " >> string (show s) >> write " #-}"

instance Pretty ExportSpecList where
    prettyPrint (ExportSpecList _ exports) = listAutoWrap "(" ")" "," exports

instance Pretty ExportSpec

instance Pretty ImportDecl where
    prettyPrint ImportDecl{..} = do
        alignP <- getConfig (cfgModuleAlignImports . cfgModule)
        write "import "
        when importSrc $ write "{-# SOURCE #-} "
        when importSafe $ write "safe "
        if importQualified
            then write "qualified "
            else when (alignP && not importSrc && not importSafe) $ write "          "
        string $ moduleName importModule
        mayM_ importAs $ \name -> do
            write " as "
            pretty name
        mayM_ importSpecs $ withPrefix space pretty

instance Pretty ImportSpecList where
    prettyPrint (ImportSpecList _ hiding specs) = do
        sortP <- getConfig (cfgModuleSortImportLists . cfgModule)
        let specs' = if sortP then sortOn HSE.prettyPrint specs else specs
        when hiding $ write "hiding "
        listAutoWrap "(" ")" "," specs'

instance Pretty ImportSpec

instance Pretty Assoc

instance Pretty Decl where
    prettyPrint (TypeDecl _ declhead ty) = depend "type" $ do
        pretty declhead
        operator "="
        pretty ty

    prettyPrint (TypeFamDecl _ declhead mresultsig minjectivityinfo) =
        depend "type family" $ do
            pretty declhead
            mayM_ mresultsig pretty
            mayM_ minjectivityinfo pretty

    prettyPrint (ClosedTypeFamDecl _ declhead mresultsig minjectivityinfo typeeqns) =
        depend "type family" $ do
            pretty declhead
            mayM_ mresultsig pretty
            mayM_ minjectivityinfo pretty
            write " where"
            newline
            lined typeeqns

    prettyPrint (DataDecl _ dataornew mcontext declhead qualcondecls mderiving) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null qualcondecls) $ list' "=" "|" qualcondecls
        mapM_ pretty mderiving

    prettyPrint (GDataDecl _ dataornew mcontext declhead mkind gadtdecls mderiving) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            mayM_ mkind $ \kind -> do
                operator "::"
                pretty kind
            write " where"
            newline
            lined gadtdecls
        mapM_ pretty mderiving

    prettyPrint (DataFamDecl _ mcontext declhead mresultsig) =
        depend "data family" $ do
            mapM_ pretty mcontext
            pretty declhead
            mapM_ pretty mresultsig

    prettyPrint (TypeInsDecl _ ty ty') = depend "type instance" $ do
        pretty ty
        operator "="
        pretty ty'

    prettyPrint (DataInsDecl _ dataornew ty qualcondecls mderiving) = do
        depend' (pretty dataornew >> write " instance") $ do
            pretty ty
            list' "=" "|" qualcondecls
        mapM_ pretty mderiving

    prettyPrint (GDataInsDecl _ dataornew ty mkind gadtdecls mderiving) = do
        depend' (pretty dataornew >> write " instance") $ do
            pretty ty
            mayM_ mkind $ \kind -> do
                operator "::"
                pretty kind
            write " where"
            newline
            lined gadtdecls
        mapM_ pretty mderiving

    prettyPrint (ClassDecl _ mcontext declhead fundeps mclassdecls) = do
        depend "class" $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null fundeps) $ list' "|" "," fundeps
        mayM_ mclassdecls $ \decls -> do
            write " where"
            newline
            indented $ prettyDecls (\d _ -> skipBlankClassDecl d) decls

    prettyPrint (InstDecl _ moverlap instrule minstdecls) = do
        depend "instance" $ do
            mapM_ pretty moverlap
            pretty instrule
        mayM_ minstdecls $ \decls -> do
            write " where"
            newline
            indented $ prettyDecls (\d _ -> skipBlankInstDecl d) decls

    prettyPrint (DerivDecl _ moverlap instrule) = depend "deriving instance" $ do
        mayM_ moverlap $ withPostfix space pretty
        pretty instrule

    prettyPrint (InfixDecl _ assoc mint ops) = do
        pretty assoc
        mayM_ mint $ withPrefix space (int . fromIntegral)
        space
        inter comma $ map prettyHSE ops

    prettyPrint (DefaultDecl _ types) = do
        write "default "
        listAutoWrap "(" ")" "," types

    prettyPrint (SpliceDecl _ expr) = pretty expr

    prettyPrint (TypeSig _ names ty) = do
        inter comma $ map pretty names
        indented $ do
            operator "::"
            pretty ty

    prettyPrint (PatSynSig _ name mtyvarbinds mcontext mcontext' ty) = depend "pattern" $ do
        pretty name
        operator "::"
        mapM_ prettyForall mtyvarbinds
        mayM_ mcontext pretty
        mayM_ mcontext' pretty
        pretty ty

    prettyPrint (FunBind _ matches) = lined matches

    prettyPrint (PatBind _ pat rhs mbinds) = do
        pretty pat
        pretty rhs
        mapM_ pretty mbinds

    prettyPrint (PatSyn _ pat pat' patternsyndirection) = do
        depend "pattern" $ do
            pretty pat
            operator sep
            pretty pat'
        case patternsyndirection of
            ExplicitBidirectional _ decls ->
                pretty (BDecls noNodeInfo decls)
            _ -> return ()
      where
        sep = case patternsyndirection of
            ImplicitBidirectional -> "="
            ExplicitBidirectional _ _ -> "<-"
            Unidirectional -> "<-"

    prettyPrint (ForImp _ callconv msafety mstring name ty) =
        depend "foreign import" $ do
            pretty callconv
            mayM_ msafety $ withPrefix space pretty
            mayM_ mstring $ withPrefix space (string . show)
            space
            pretty name
            operator "::"
            pretty ty

    prettyPrint (ForExp _ callconv mstring name ty) = depend "foreign export" $ do
        pretty callconv
        mayM_ mstring $ withPrefix space (string . show)
        space
        pretty name
        operator "::"
        pretty ty

    prettyPrint (RulePragmaDecl _ rules) =
        if null rules
        then prettyPragma' "RULES" Nothing
        else prettyPragma "RULES" $ mapM_ pretty rules

    prettyPrint (DeprPragmaDecl _ deprecations) =
        if null deprecations
        then prettyPragma' "DEPRECATED" Nothing
        else prettyPragma "DEPRECATED" $
            forM_ deprecations $ \(names, str) -> do
                unless (null names) $ do
                    inter comma $ map pretty names
                    space
                string (show str)

    prettyPrint (WarnPragmaDecl _ warnings) =
        if null warnings
        then prettyPragma' "WARNING" Nothing
        else prettyPragma "WARNING" $
            forM_ warnings $ \(names, str) -> do
                unless (null names) $ do
                    inter comma $ map pretty names
                    space
                string (show str)

    prettyPrint (InlineSig _ inline mactivation qname) = prettyPragma name $ do
        mayM_ mactivation $ withPostfix space pretty
        pretty qname
      where
        name = if inline then "INLINE" else "NOINLINE"

    prettyPrint (InlineConlikeSig _ mactivation qname) =
        prettyPragma "INLINE CONLIKE" $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname

    prettyPrint (SpecSig _ mactivation qname types) =
        prettyPragma "SPECIALISE" $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname
            operator "::"
            inter comma $ map pretty types

    prettyPrint (SpecInlineSig _ inline mactivation qname types) =
        prettyPragma name $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname
            operator "::"
            inter comma $ map pretty types
      where
        name = if inline then "SPECIALISE INLINE" else "SPECIALISE NOINLINE"

    prettyPrint (InstSig _ instrule) = prettyPragma "SPECIALISE instance" $ pretty instrule

    prettyPrint (AnnPragma _ annotation) = prettyPragma "ANN" $ pretty annotation

    prettyPrint (MinimalPragma _ mbooleanformula) = prettyPragma "MINIMAL" $
        mapM_ pretty mbooleanformula

    -- prettyPrint (RoleAnnotDecl _ qname roles) = undefined

    prettyPrint decl = prettyHSE decl

instance Pretty DeclHead where
    prettyPrint (DHead _ name) = pretty name

    prettyPrint (DHInfix _ tyvarbind name) = do
        pretty tyvarbind
        pretty $ VarOp noNodeInfo name

    prettyPrint (DHParen _ declhead) = parens $ pretty declhead

    prettyPrint (DHApp _ declhead tyvarbind) = depend' (pretty declhead) $ pretty tyvarbind

instance Pretty InstRule where
    prettyPrint (IRule _ mtyvarbinds mcontext insthead) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty insthead

    prettyPrint (IParen _ instrule) = parens $ pretty instrule

instance Pretty InstHead where
    prettyPrint (IHCon _ qname) = pretty qname

    prettyPrint (IHInfix _ ty qname) = do
        pretty ty
        space
        pretty qname

    prettyPrint (IHParen _ insthead) = parens $ pretty insthead

    prettyPrint (IHApp _ insthead ty) = depend' (pretty insthead) $ pretty ty

instance Pretty Binds where
    prettyPrint (BDecls _ decls) = do
        newline
        write "  where"
        newline
        indented $ prettyDecls (\d _ -> skipBlankDecl d) decls

    prettyPrint (IPBinds _ ipbinds) = do
        newline
        write "  where"
        newline
        indented $ lined ipbinds

instance Pretty IPBind where
    prettyPrint (IPBind _ ipname expr) = do
        pretty ipname
        operator "="
        pretty expr

instance Pretty InjectivityInfo where
    prettyPrint (InjectivityInfo _ name names) = do
        operator "|"
        pretty name
        operator "->"
        inter space $ map pretty names

instance Pretty ResultSig where
    prettyPrint (KindSig _ kind) = do
        operator "::"
        pretty kind

    prettyPrint (TyVarSig _ tyvarbind) = do
        operator "="
        pretty tyvarbind

instance Pretty ClassDecl where
    prettyPrint (ClsDecl _ decl) = pretty decl

    prettyPrint (ClsDataFam _ mcontext declhead mresultsig) = depend "data" $ do
        mapM_ pretty mcontext
        pretty declhead
        mayM_ mresultsig pretty

    prettyPrint (ClsTyFam _ declhead mresultsig minjectivityinfo) =
        depend "type" $ do
            pretty declhead
            mayM_ mresultsig pretty
            mapM_ pretty minjectivityinfo

    prettyPrint (ClsTyDef _ typeeqn) = depend "type" $ pretty typeeqn

    prettyPrint (ClsDefSig _ name ty) = depend "default" $ do
        pretty name
        operator "::"
        pretty ty

instance Pretty InstDecl where
    prettyPrint (InsDecl _ decl) = pretty decl

    prettyPrint (InsType _ ty ty') = depend "type" $ do
        pretty ty
        operator "="
        pretty ty'

    prettyPrint (InsData _ dataornew ty qualcondecls mderiving) =
        depend' (pretty dataornew) $ do
            pretty ty
            unless (null qualcondecls) $ list' "=" "|" qualcondecls
            mapM_ pretty mderiving

    prettyPrint (InsGData _ dataornew ty mkind gadtdecls mderiving) =
        depend' (pretty dataornew) $ do
            pretty ty
            mayM_ mkind $ withPrefix space pretty
            unless (null gadtdecls) $ list' "=" "|" gadtdecls
            mapM_ pretty mderiving

instance Pretty Deriving where
    prettyPrint (Deriving _ instrules) = do
        newline
        indented $ do
            write "deriving "
            case instrules of
                [ i@IRule{} ] -> pretty i
                _ -> listAutoWrap "(" ")" "," instrules'
      where
        instrules' = case instrules of
            [ IParen _ i ] -> [ i ]
            _ -> instrules

instance Pretty ConDecl where
    prettyPrint (ConDecl _ name types) = do
        pretty name
        unless (null types) $ do
            space
            oneline flex <|> vertical
      where
        flex = inter space $ map pretty types
        vertical = aligned $ lined types

    prettyPrint (InfixConDecl _ ty name ty') = do
        pretty ty
        pretty $ ConOp noNodeInfo name
        pretty ty'

    prettyPrint (RecDecl _ name fielddecls) = do
        pretty name
        sepSpace
        oneline flex <|> vertical
      where
        flex = listH "{" "}" "," fielddecls
        vertical = listV "{" "}" "," fielddecls

instance Pretty FieldDecl where
    prettyPrint (FieldDecl _ names ty) = do
        inter comma $ map pretty names
        indented $ do
            operator "::"
            pretty ty

instance Pretty QualConDecl where
    prettyPrint (QualConDecl _ mtyvarbinds mcontext condecl) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty condecl

instance Pretty GadtDecl where
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
        pretty name
        operator "::"
        mayM_ mfielddecls $ \decls -> do
            oneline (flex decls) <|> vertical decls
            operator "->"
        pretty ty
      where
        flex = listH "{" "}" ","
        vertical = listV "{" "}" ","

instance Pretty Match where
    prettyPrint (Match _ name pats rhs mbinds) = do
        pretty name
        unless (null pats) $ do
            space
            inter space $ map pretty pats
        pretty rhs
        mapM_ pretty mbinds

    prettyPrint (InfixMatch _ pat name pats rhs mbinds) = do
        pretty pat
        pretty $ VarOp noNodeInfo name
        inter space $ map pretty pats
        pretty rhs
        mapM_ pretty mbinds

instance Pretty Rhs where
    prettyPrint (UnGuardedRhs _ expr) = cut . indented $ do
        operator "="
        pretty expr

    prettyPrint (GuardedRhss _ guardedrhss) = aligned $ lined guardedrhss

instance Pretty GuardedRhs where
    prettyPrint (GuardedRhs _ stmts expr) = do
        operator "|"
        inter comma $ map pretty stmts
        indented $ do
            operator "="
            pretty expr

instance Pretty Context where
    prettyPrint (CxSingle _ asst) = do
        pretty asst
        operator "=>"

    prettyPrint (CxTuple _ assts) = do
        list "(" ")" "," assts
        operator "=>"

    prettyPrint (CxEmpty _) = do
        write "()"
        operator "=>"

instance Pretty FunDep where
    prettyPrint (FunDep _ names names') = do
        inter space $ map pretty names
        operator "->"
        inter space $ map pretty names'

instance Pretty Asst where
    prettyPrint (ClassA _ qname types) = do
        pretty qname
        space
        inter space $ map pretty types

    prettyPrint (AppA _ name types) = do
        pretty name
        space
        inter space $ map pretty types

    prettyPrint (InfixA _ ty qname ty') = do
        pretty ty
        pretty $ QVarOp noNodeInfo qname
        pretty ty'

    prettyPrint (IParam _ ipname ty) = do
        pretty ipname
        operator "::"
        pretty ty

    prettyPrint (EqualP _ ty ty') = do
        pretty ty
        operator "~"
        pretty ty'

    prettyPrint (ParenA _ asst) = parens $ pretty asst

    prettyPrint (WildCardA _ mname) = do
        write "_"
        mapM_ pretty mname

instance Pretty Type where
    prettyPrint (TyForall _ mtyvarbinds mcontext ty) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty ty

    prettyPrint (TyFun _ ty ty') = do
        pretty ty
        operator "->"
        pretty ty'

    prettyPrint (TyTuple _ boxed tys) = case boxed of
        Unboxed -> list "(#" "#)" "," tys
        Boxed -> list "(" ")" "," tys

    prettyPrint (TyList _ ty) = group "[" "]" $ pretty ty

    prettyPrint (TyParArray _ ty) = group "[:" ":]" $ pretty ty

    prettyPrint (TyApp _ ty ty') = do
        pretty ty
        space
        pretty ty'

    prettyPrint (TyVar _ name) = pretty name

    prettyPrint (TyCon _ qname) = pretty qname

    prettyPrint (TyParen _ ty) = parens $ pretty ty

    prettyPrint (TyInfix _ ty qname ty') = do
        pretty ty
        pretty $ QVarOp noNodeInfo qname
        pretty ty'

    prettyPrint (TyKind _ ty kind) = do
        pretty ty
        operator "::"
        pretty kind

    prettyPrint t@(TyPromoted _ _promoted) = prettyHSE t

    prettyPrint (TyEquals _ ty ty') = do
        pretty ty
        operator "~"
        pretty ty'

    prettyPrint (TySplice _ splice) = pretty splice

    prettyPrint (TyBang _ bangtype unpackedness ty) = do
        pretty unpackedness
        pretty bangtype
        pretty ty

    prettyPrint t@(TyWildCard _ _mname) = prettyHSE t

    prettyPrint (TyQuasiQuote _ str str') = do
        write "["
        string str
        write "|"
        string str'
        write "|]"

instance Pretty Kind where
    prettyPrint (KindStar _) = write "*"

    prettyPrint (KindFn _ kind kind') = do
        pretty kind
        operator "->"
        pretty kind'

    prettyPrint (KindParen _ kind) = parens $ pretty kind

    prettyPrint (KindVar _ qname) = pretty qname

    prettyPrint (KindApp _ kind kind') = do
        pretty kind
        space
        pretty kind'

    prettyPrint (KindTuple _ kinds) = list "'(" ")" "," kinds

    prettyPrint (KindList _ kind) = group "'[" "]" $ pretty kind

instance Pretty TyVarBind where
    prettyPrint (KindedVar _ name kind) = parens $ do
        pretty name
        operator "::"
        pretty kind

    prettyPrint (UnkindedVar _ name) = pretty name

instance Pretty TypeEqn where
    prettyPrint (TypeEqn _ ty ty') = do
        pretty ty
        operator "="
        pretty ty'

instance Pretty Exp where
    -- prettyPrint (Var _ qname) = undefined

    -- prettyPrint (OverloadedLabel _ str) = undefined

    -- prettyPrint (IPVar _ ipname) = undefined

    -- prettyPrint (Con _ qname) = undefined

    -- prettyPrint (Lit _ literal) = undefined

    prettyPrint (InfixApp _ expr qop expr') = do
        pretty expr
        pretty qop
        pretty expr'

    prettyPrint (App _ expr expr') = do
        pretty expr
        spaceOrNewline
        pretty expr'

    prettyPrint (NegApp _ expr) = do
        write "-"
        pretty expr

    prettyPrint (Lambda _ pats expr) = do
        write "\\"
        space
        inter space $ map pretty pats
        operator "->"
        pretty expr

    prettyPrint (Let _ binds expr) = aligned $ do
        write "let "
        aligned $ pretty (CompactBinds binds)
        spaceOrNewline
        write "in "
        pretty expr

    prettyPrint (If _ expr expr' expr'') = do
        write "if "
        indented $ pretty expr
        spaceOrNewline
        write "then "
        indented $ pretty expr'
        spaceOrNewline
        write "else "
        indented $ pretty expr''

    prettyPrint (MultiIf _ guardedrhss) = do
        write "if"
        newline
        indented . lined $ map GuardedAlt guardedrhss

    prettyPrint (Case _ expr alts) = do
        write "case "
        pretty expr
        write " of"
        if null alts
            then write " { }"
            else do
                newline
                indented $ lined alts

    prettyPrint (Do _ stmts) = do
        write "do"
        newline
        indented $ lined stmts

    prettyPrint (MDo _ stmts) = do
        write "mdo"
        newline
        indented $ lined stmts

    -- prettyPrint (Tuple _ boxed exprs) = undefined

    -- prettyPrint (TupleSection _ boxed mexprs) = undefined

    -- prettyPrint (List _ exprs) = undefined

    -- prettyPrint (ParArray _ exprs) = undefined

    -- prettyPrint (Paren _ expr) = undefined

    -- prettyPrint (LeftSection _ expr qop) = undefined

    -- prettyPrint (RightSection _ qop expr) = undefined

    -- prettyPrint (RecConstr _ qname fieldupdates) = undefined

    -- prettyPrint (RecUpdate _ expr fieldupdates) = undefined

    -- prettyPrint (EnumFrom _ expr) = undefined

    -- prettyPrint (EnumFromTo _ expr expr') = undefined

    -- prettyPrint (EnumFromThen _ expr expr') = undefined

    -- prettyPrint (EnumFromThenTo _ expr expr' expr'') = undefined

    -- prettyPrint (ParArrayFromTo _ expr expr') = undefined

    -- prettyPrint (ParArrayFromThenTo _ expr expr' expr'') = undefined

    -- prettyPrint (ListComp _ expr qualstmts) = undefined

    -- prettyPrint (ParComp _ expr qualstmtss) = undefined

    -- prettyPrint (ParArrayComp _ expr qualstmtss) = undefined

    -- prettyPrint (ExpTypeSig _ expr typ) = undefined

    -- prettyPrint (VarQuote _ qname) = undefined

    -- prettyPrint (TypQuote _ qname) = undefined

    -- prettyPrint (BracketExp _ bracket) = undefined

    -- prettyPrint (SpliceExp _ splice) = undefined

    -- prettyPrint (QuasiQuote _ str str') = undefined

    -- prettyPrint (TypeApp _ typ) = undefined

    -- prettyPrint (XTag _ xname xattrs mexpr exprs) = undefined

    -- prettyPrint (XETag _ xname xattrs mexpr) = undefined

    -- prettyPrint (XPcdata _ str) = undefined

    -- prettyPrint (XExpTag _ expr) = undefined

    -- prettyPrint (XChildTag _ exprs) = undefined

    -- prettyPrint (CorePragma _ str expr) = undefined

    -- prettyPrint (SCCPragma _ str expr) = undefined

    -- prettyPrint (GenPragma _ str intp intp' expr) = undefined

    -- prettyPrint (Proc _ pat expr) = undefined

    -- prettyPrint (LeftArrApp _ expr expr') = undefined

    -- prettyPrint (RightArrApp _ expr expr') = undefined

    -- prettyPrint (LeftArrHighApp _ expr expr') = undefined

    -- prettyPrint (RightArrHighApp _ expr expr') = undefined

    prettyPrint (LCase _ alts) = do
        write "\\case"
        if null alts
            then write " { }"
            else do
                newline
                indented $ lined alts

    -- prettyPrint (ExprHole _) = undefined

    prettyPrint e = prettyHSE e

instance Pretty Alt where
    prettyPrint (Alt _ pat rhs mbinds) = do
        pretty pat
        pretty $ GuardedAlts rhs
        mapM_ pretty mbinds

instance Pretty Stmt where
    prettyPrint (Generator _ pat expr) = do
        pretty pat
        operator "<-"
        pretty expr

    prettyPrint (Qualifier _ expr) = pretty expr

    prettyPrint (LetStmt _ binds) = do
        write "let "
        pretty $ CompactBinds binds

    prettyPrint (RecStmt _ stmts) = do
        write "rec "
        aligned $ lined stmts

instance Pretty QOp where
    prettyPrint qop = withOperatorFormatting (opName qop) (prettyHSE qop) (return ())
      where
        opName (QVarOp _ qname) = opName' qname
        opName (QConOp _ qname) = opName' qname

        opName' (Qual _ _ (Ident _ _)) = "``"
        opName' (Qual _ _ (Symbol _ _)) = ""
        opName' (UnQual _ (Ident _ _)) = "``"
        opName' (UnQual _ (Symbol _ str)) = BS8.pack str
        opName' (Special _ (FunCon _)) = "->"
        opName' (Special _ (Cons _)) = ":"
        opName' (Special _ _) = ""

instance Pretty Op where
    prettyPrint (VarOp l name) = prettyPrint (QVarOp l (UnQual noNodeInfo name))
    prettyPrint (ConOp l name) = prettyPrint (QConOp l (UnQual noNodeInfo name))

instance Pretty ModulePragma where
    prettyPrint (LanguagePragma _ names) =
        prettyPragma "LANGUAGE" . inter comma $ map pretty names

    prettyPrint (OptionsPragma _ mtool str) = prettyPragma name $ string str
      where
        name = case mtool of
            Just tool -> "OPTIONS_" `mappend` BS8.pack (HSE.prettyPrint tool)
            Nothing -> "OPTIONS"

    prettyPrint (AnnModulePragma _ annotation) = prettyPragma "ANN" $ pretty annotation

instance Pretty Rule where
    prettyPrint (Rule _ str mactivation mrulevars expr expr') = do
        string (show str)
        space
        mayM_ mactivation $ withPostfix space pretty
        mapM_ prettyForall mrulevars
        pretty expr
        operator "="
        pretty expr'

instance Pretty RuleVar where
    prettyPrint (RuleVar _ name) = pretty name

    prettyPrint (TypedRuleVar _ name ty) = parens $ do
        pretty name
        operator "::"
        pretty ty

instance Pretty Activation where
    prettyPrint (ActiveFrom _ pass) = brackets . int $ fromIntegral pass

    prettyPrint (ActiveUntil _ pass) = brackets $ do
        write "~"
        int $ fromIntegral pass

instance Pretty Annotation where
    prettyPrint (Ann _ name expr) = do
        pretty name
        space
        pretty expr

    prettyPrint (TypeAnn _ name expr) = do
        write "type "
        pretty name
        space
        pretty expr

    prettyPrint (ModuleAnn _ expr) = do
        write "module "
        pretty expr

instance Pretty BooleanFormula where
    prettyPrint (VarFormula _ name) = pretty name

    prettyPrint (AndFormula _ booleanformulas) =
        inter comma $ map pretty booleanformulas

    prettyPrint (OrFormula _ booleanformulas) =
        inter (operator "|") $ map pretty booleanformulas

    prettyPrint (ParenFormula _ booleanformula) = parens $ pretty booleanformula

instance Pretty Pat

instance Pretty Splice

-- Stick with HSE
instance Pretty DataOrNew

instance Pretty BangType

instance Pretty Unpackedness

instance Pretty ModuleName

instance Pretty QName

instance Pretty Name

instance Pretty IPName

instance Pretty Safety

instance Pretty CallConv

instance Pretty Overlap

-- Helpers
newtype GuardedAlt l = GuardedAlt (GuardedRhs l)
    deriving (Functor, Annotated)

instance Pretty GuardedAlt where
    prettyPrint (GuardedAlt (GuardedRhs _ stmts expr)) = do
        operator "|"
        inter comma $ map pretty stmts
        operator "->"
        indented $ pretty expr

newtype GuardedAlts l = GuardedAlts (Rhs l)
    deriving (Functor, Annotated)

instance Pretty GuardedAlts where
    prettyPrint (GuardedAlts (UnGuardedRhs _ expr)) = cut . indented $ do
        operator "->"
        pretty expr

    prettyPrint (GuardedAlts (GuardedRhss _ guardedrhss)) =
        aligned . lined $ map GuardedAlt guardedrhss

newtype CompactBinds l = CompactBinds (Binds l)
    deriving (Functor, Annotated)

instance Pretty CompactBinds where
    prettyPrint (CompactBinds (BDecls _ decls)) = aligned $ lined decls
    prettyPrint (CompactBinds (IPBinds _ ipbinds)) = aligned $ lined ipbinds

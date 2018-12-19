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

flattenApp :: Annotated ast
           => (ast NodeInfo -> Maybe (ast NodeInfo, ast NodeInfo))
           -> ast NodeInfo
           -> [ast NodeInfo]
flattenApp fn = go . amap (\info -> info { nodeInfoComments = [] })
  where
    go x = case fn x of
        Just (lhs, rhs) -> let lhs' = go $ copyComments Before x lhs
                               rhs' = go $ copyComments After x rhs
                           in
                               lhs' ++ rhs'
        Nothing -> [ x ]

flattenInfix :: (Annotated ast1, Annotated ast2)
             => (ast1 NodeInfo
                 -> Maybe (ast1 NodeInfo, ast2 NodeInfo, ast1 NodeInfo))
             -> ast1 NodeInfo
             -> (ast1 NodeInfo, [(ast2 NodeInfo, ast1 NodeInfo)])
flattenInfix fn = go . amap (\info -> info { nodeInfoComments = [] })
  where
    go x = case fn x of
        Just (lhs, op, rhs) -> let (lhs', ops) = go $ copyComments Before x lhs
                                   (lhs'', ops') =
                                       go $ copyComments After x rhs
                               in
                                   (lhs', ops ++ (op, lhs'') : ops')
        Nothing -> (x, [])

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

-- | Return comments with matching location.
filterComments :: Annotated a
               => (Maybe ComInfoLocation -> Bool)
               -> a NodeInfo
               -> [ComInfo]
filterComments f = filter (f . comInfoLocation) . nodeInfoComments . ann

-- | Copy comments from one AST node to another.
copyComments :: (Annotated ast1, Annotated ast2)
             => ComInfoLocation
             -> ast1 NodeInfo
             -> ast2 NodeInfo
             -> ast2 NodeInfo
copyComments loc from to = amap updateComments to
  where
    updateComments info = info { nodeInfoComments = oldComments ++ newComments
                               }
    oldComments = filterComments (/= Just loc) to
    newComments = filterComments (== Just loc) from

-- | Return the configuration name of an operator
opName :: QOp a -> ByteString
opName op = case op of
    (QVarOp _ qname) -> opName' qname
    (QConOp _ qname) -> opName' qname

-- | Return the configuration name of an operator
opName' :: QName a -> ByteString
opName' (Qual _ _ (Ident _ _)) = "``"
opName' (Qual _ _ (Symbol _ _)) = ""
opName' (UnQual _ (Ident _ _)) = "``"
opName' (UnQual _ (Symbol _ str)) = BS8.pack str
opName' (Special _ (FunCon _)) = "->"
opName' (Special _ (Cons _)) = ":"
opName' (Special _ _) = ""

lined :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer FlexConfig ()
lined = inter newline . map (cut . pretty)

listVinternal :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> ByteString
              -> [ast NodeInfo]
              -> Printer FlexConfig ()
listVinternal ctx sep xs = aligned $ do
    ws <- getConfig (cfgOpWs ctx sep . cfgOp)
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
                column sepCol $ operatorV ctx sep
                cut $ prettyPrint x'
                printComments After x'
  where
    printCommentsSimple loc ast = let comments = map comInfoComment $
                                          filterComments (== Just loc) ast
                                  in
                                      forM_ comments $ \comment -> do
                                          printComment (Just $ srcInfoSpan $
                                                            nodeInfoSpan $ ann ast)
                                                       comment

listH :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
listH _ open close _ [] = do
    write open
    write close

listH ctx open close sep xs =
    groupH ctx open close . inter (operatorH ctx sep) $ map pretty xs

listV :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
listV ctx open close sep xs = groupV ctx open close $ do
    ws <- getConfig (cfgOpWs ctx sep . cfgOp)
    ws' <- getConfig (cfgGroupWs ctx open . cfgGroup)
    unless ((wsLinebreak Before ws')
            || (wsSpace After ws')
            || (wsLinebreak After ws)
            || not (wsSpace After ws))
           space
    listVinternal ctx sep xs

list :: (Annotated ast, Pretty ast)
     => LayoutContext
     -> ByteString
     -> ByteString
     -> ByteString
     -> [ast NodeInfo]
     -> Printer FlexConfig ()
list ctx open close sep xs = oneline hor <|> ver
  where
    hor = listH ctx open close sep xs
    ver = listV ctx open close sep xs

listH' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> ByteString
       -> ByteString
       -> [ast NodeInfo]
       -> Printer FlexConfig ()
listH' ctx op sep xs = do
    operator ctx op
    inter (operatorH ctx sep) $ map pretty xs

listV' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> ByteString
       -> ByteString
       -> [ast NodeInfo]
       -> Printer FlexConfig ()
listV' ctx op sep xs = do
    operator ctx op
    listVinternal ctx sep xs

list' :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> ByteString
      -> ByteString
      -> [ast NodeInfo]
      -> Printer FlexConfig ()
list' ctx op sep xs = oneline hor <|> ver
  where
    hor = listH' ctx op sep xs
    ver = listV' ctx op sep xs

listAutoWrap :: (Annotated ast, Pretty ast)
             => LayoutContext
             -> ByteString
             -> ByteString
             -> ByteString
             -> [ast NodeInfo]
             -> Printer FlexConfig ()
listAutoWrap _ open close _ [] = do
    write open
    write close

listAutoWrap ctx open close sep (x : xs) =
    aligned . groupH ctx open close . aligned $ do
        ws <- getConfig (cfgOpWs ctx sep . cfgOp)
        let correction = if wsLinebreak After ws
                         then 0
                         else BS.length sep + if wsSpace After ws then 1 else 0
        col <- getNextColumn
        pretty x
        forM_ xs $ \x' -> do
            printComments Before x'
            cut $ do
                column (col - fromIntegral correction) $ operator ctx sep
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

skipBlank :: Annotated ast
          => (ast NodeInfo -> ast NodeInfo -> Bool)
          -> ast NodeInfo
          -> ast NodeInfo
          -> Bool
skipBlank skip a b =
    skip a b && null (comments After a) && null (comments Before b)
  where
    comments loc ast = filterComments (== Just loc) ast

skipBlankAfterDecl :: Decl a -> Bool
skipBlankAfterDecl a = case a of
    TypeSig{} -> True
    DeprPragmaDecl{} -> True
    WarnPragmaDecl{} -> True
    AnnPragma{} -> True
    MinimalPragma{} -> True
    InlineSig{} -> True
    InlineConlikeSig{} -> True
    SpecSig{} -> True
    SpecInlineSig{} -> True
    InstSig{} -> True
    PatSynSig{} -> True
    _ -> False

skipBlankDecl :: Decl NodeInfo -> Decl NodeInfo -> Bool
skipBlankDecl = skipBlank $ \a _ -> skipBlankAfterDecl a

skipBlankClassDecl :: ClassDecl NodeInfo -> ClassDecl NodeInfo -> Bool
skipBlankClassDecl = skipBlank $ \a _ -> case a of
    (ClsDecl _ decl) -> skipBlankAfterDecl decl
    ClsTyDef{} -> True
    ClsDefSig{} -> True
    _ -> False

skipBlankInstDecl :: InstDecl NodeInfo -> InstDecl NodeInfo -> Bool
skipBlankInstDecl = skipBlank $ \a _ -> case a of
    (InsDecl _ decl) -> skipBlankAfterDecl decl
    _ -> False

prettyDecls :: (Annotated ast, Pretty ast)
            => (ast NodeInfo -> ast NodeInfo -> Bool)
            -> [ast NodeInfo]
            -> Printer FlexConfig ()
prettyDecls fn = inter blankline . map lined . runs fn

prettySimpleDecl :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
                 => ast1 NodeInfo
                 -> ByteString
                 -> ast2 NodeInfo
                 -> Printer FlexConfig ()
prettySimpleDecl lhs op rhs = withLayout cfgLayoutDeclaration flex vertical
  where
    flex = do
        pretty lhs
        operator Declaration op
        pretty rhs
    vertical = do
        pretty lhs
        operatorV Declaration op
        pretty rhs

prettyForall :: (Annotated ast, Pretty ast)
             => [ast NodeInfo]
             -> Printer FlexConfig ()
prettyForall vars = do
    write "forall "
    inter space $ map pretty vars
    write "."
    sepSpace

prettyTypesig :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> [ast NodeInfo]
              -> Type NodeInfo
              -> Printer FlexConfig ()
prettyTypesig ctx names ty = withLayout cfgLayoutTypesig flex vertical
  where
    flex = do
        inter comma $ map pretty names
        onside $ do
            operator ctx "::"
            pretty ty

    vertical = do
        inter comma $ map pretty names
        onside . alignOnOperator ctx "::" $ pretty' ty

    pretty' (TyForall _ mtyvarbinds mcontext ty') = do
        forM_ mtyvarbinds $ \tyvarbinds -> do
            write "forall "
            inter space $ map pretty tyvarbinds
            withOperatorFormattingV Type "=>" (write ". ") id
        forM_ mcontext $ \context -> do
            case context of
                (CxSingle _ asst) -> pretty asst
                (CxTuple _ assts) -> list Type "(" ")" "," assts
                (CxEmpty _) -> write "()"
            operatorV Type "=>"
        pretty' ty'
    pretty' (TyFun _ ty' ty'') = do
        pretty ty'
        operatorV Type "->"
        pretty' ty''
    pretty' ty' = pretty ty'

prettyApp :: (Annotated ast1, Annotated ast2, Pretty ast1, Pretty ast2)
          => ast1 NodeInfo
          -> [ast2 NodeInfo]
          -> Printer FlexConfig ()
prettyApp fn args = withLayout cfgLayoutApp flex vertical
  where
    flex = do
        pretty fn
        forM_ args $ \arg -> cut $ do
            spaceOrNewline
            pretty arg

    vertical = do
        pretty fn
        space
        aligned . inter newline $ map pretty args

prettyInfixApp :: (Annotated ast, Pretty ast, HSE.Pretty (op NodeInfo))
               => (op NodeInfo -> ByteString)
               -> LayoutContext
               -> (ast NodeInfo, [(op NodeInfo, ast NodeInfo)])
               -> Printer FlexConfig ()
prettyInfixApp nameFn ctx (lhs, args) = withLayout cfgLayoutInfixApp flex vertical
  where
    flex = do
        pretty lhs
        forM_ args $ \(op, arg) -> cut $ do
            withOperatorFormatting ctx (nameFn op) (prettyHSE op) id
            pretty arg

    vertical = do
        pretty lhs
        forM_ args $ \(op, arg) -> do
            withOperatorFormattingV ctx (nameFn op) (prettyHSE op) id
            pretty arg

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
                  , ifNotEmpty (prettyDecls skipBlankDecl) decls
                  ]
      where
        ifNotEmpty f xs = if null xs then Nothing else Just (f xs)

    prettyPrint ast@XmlPage{} = prettyHSE ast
    prettyPrint ast@XmlHybrid{} = prettyHSE ast

instance Pretty ModuleHead where
    prettyPrint (ModuleHead _ name mwarning mexports) = do
        depend "module" $ do
            pretty name
            mayM_ mwarning $ withPrefix spaceOrNewline pretty
        mayM_ mexports pretty
        write " where"

instance Pretty WarningText where
    prettyPrint (DeprText _ s) = write "{-# DEPRECATED " >> string (show s) >> write " #-}"
    prettyPrint (WarnText _ s) = write "{-# WARNING " >> string (show s) >> write " #-}"

instance Pretty ExportSpecList where
    prettyPrint (ExportSpecList _ exports) =
        withLayout cfgLayoutExportSpecList flex vertical
      where
        flex = do
            space
            listAutoWrap Other "(" ")" "," exports
        vertical = withIndent cfgIndentExportSpecList $
            listV Other "(" ")" "," exports

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
        mayM_ importSpecs pretty

instance Pretty ImportSpecList where
    prettyPrint (ImportSpecList _ hiding specs) = do
        sortP <- getConfig (cfgModuleSortImportLists . cfgModule)
        let specs' = if sortP then sortOn HSE.prettyPrint specs else specs
        when hiding $ write " hiding"
        withLayout cfgLayoutImportSpecList (flex specs') (vertical specs')
      where
        flex imports = do
            space
            listAutoWrap Other "(" ")" "," imports
        vertical imports = withIndent cfgIndentImportSpecList $
            listAutoWrap Other "(" ")" "," imports

instance Pretty ImportSpec

instance Pretty Assoc

instance Pretty Decl where
    prettyPrint (TypeDecl _ declhead ty) =
        depend "type" $ prettySimpleDecl declhead "=" ty

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

    prettyPrint (DataDecl _ dataornew mcontext declhead qualcondecls derivings) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null qualcondecls) $ withLayout cfgLayoutDeclaration flex vertical
        mapM_ pretty derivings
      where
        flex = list' Declaration "=" "|" qualcondecls
        vertical = listV' Declaration "=" "|" qualcondecls

    prettyPrint (GDataDecl _ dataornew mcontext declhead mkind gadtdecls derivings) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            mayM_ mkind $ \kind -> do
                operator Declaration "::"
                pretty kind
            write " where"
            newline
            lined gadtdecls
        mapM_ pretty derivings

    prettyPrint (DataFamDecl _ mcontext declhead mresultsig) =
        depend "data family" $ do
            mapM_ pretty mcontext
            pretty declhead
            mapM_ pretty mresultsig

    prettyPrint (TypeInsDecl _ ty ty') =
        depend "type instance" $ prettySimpleDecl ty "=" ty'

    prettyPrint (DataInsDecl _ dataornew ty qualcondecls derivings) = do
        depend' (pretty dataornew >> write " instance") $ do
            pretty ty
            withLayout cfgLayoutDeclaration flex vertical
        mapM_ pretty derivings
      where
        flex = list' Declaration "=" "|" qualcondecls
        vertical = listV' Declaration "=" "|" qualcondecls

    prettyPrint (GDataInsDecl _ dataornew ty mkind gadtdecls derivings) = do
        depend' (pretty dataornew >> write " instance") $ do
            pretty ty
            mayM_ mkind $ \kind -> do
                operator Declaration "::"
                pretty kind
            write " where"
            newline
            lined gadtdecls
        mapM_ pretty derivings

    prettyPrint (ClassDecl _ mcontext declhead fundeps mclassdecls) = do
        depend "class" $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null fundeps) $ list' Declaration "|" "," fundeps
        mayM_ mclassdecls $ \decls -> do
            write " where"
            withIndent cfgIndentClass $ prettyDecls skipBlankClassDecl decls

    prettyPrint (InstDecl _ moverlap instrule minstdecls) = do
        depend "instance" $ do
            mapM_ pretty moverlap
            pretty instrule
        mayM_ minstdecls $ \decls -> do
            write " where"
            withIndent cfgIndentClass $ prettyDecls skipBlankInstDecl decls

    prettyPrint (DerivDecl _ mderivstrategy moverlap instrule) =
        depend "deriving" $ do
            mayM_ mderivstrategy $ withPostfix space pretty
            write "instance "
            mayM_ moverlap $ withPostfix space pretty
            pretty instrule

    prettyPrint (InfixDecl _ assoc mint ops) = do
        pretty assoc
        mayM_ mint $ withPrefix space (int . fromIntegral)
        space
        inter comma $ map prettyHSE ops

    prettyPrint (DefaultDecl _ types) = do
        write "default "
        listAutoWrap Other "(" ")" "," types

    prettyPrint (SpliceDecl _ expr) = pretty expr

    prettyPrint (TypeSig _ names ty) = prettyTypesig Declaration names ty

    prettyPrint (PatSynSig _ names mtyvarbinds mcontext mcontext' ty) = depend "pattern" $ do
        inter comma $ map pretty names
        operator Declaration "::"
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
        depend "pattern" $ prettySimpleDecl pat sep pat'
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
            prettyTypesig Declaration [name] ty

    prettyPrint (ForExp _ callconv mstring name ty) = depend "foreign export" $ do
        pretty callconv
        mayM_ mstring $ withPrefix space (string . show)
        space
        prettyTypesig Declaration [name] ty

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
            operator Declaration "::"
            inter comma $ map pretty types

    prettyPrint (SpecInlineSig _ inline mactivation qname types) =
        prettyPragma name $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname
            operator Declaration "::"
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
        withIndent cfgIndentWhere $ prettyDecls skipBlankDecl decls

    prettyPrint (IPBinds _ ipbinds) = do
        newline
        write "  where"
        withIndent cfgIndentWhere $ lined ipbinds

instance Pretty IPBind where
    prettyPrint (IPBind _ ipname expr) = onside $ prettySimpleDecl ipname "=" expr

instance Pretty InjectivityInfo where
    prettyPrint (InjectivityInfo _ name names) = do
        operator Declaration "|"
        pretty name
        operator Declaration "->"
        inter space $ map pretty names

instance Pretty ResultSig where
    prettyPrint (KindSig _ kind) = withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "::"
            pretty kind
        vertical = do
            operatorV Declaration "::"
            pretty kind

    prettyPrint (TyVarSig _ tyvarbind) = withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "="
            pretty tyvarbind
        vertical = do
            operatorV Declaration "="
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

    prettyPrint (ClsDefSig _ name ty) =
        depend "default" $ prettyTypesig Declaration [ name ] ty

instance Pretty InstDecl where
    prettyPrint (InsDecl _ decl) = pretty decl

    prettyPrint (InsType _ ty ty') = depend "type" $ prettySimpleDecl ty "=" ty'

    prettyPrint (InsData _ dataornew ty qualcondecls derivings) =
        depend' (pretty dataornew) $ do
            pretty ty
            unless (null qualcondecls) $ list' Declaration "=" "|" qualcondecls
            mapM_ pretty derivings

    prettyPrint (InsGData _ dataornew ty mkind gadtdecls derivings) =
        depend' (pretty dataornew) $ do
            pretty ty
            mayM_ mkind $ withPrefix space pretty
            unless (null gadtdecls) $ list' Declaration "=" "|" gadtdecls
            mapM_ pretty derivings

instance Pretty Deriving where
    prettyPrint (Deriving _ mderivstrategy instrules) = do
        newline
        indented $ do
            write "deriving "
            mayM_ mderivstrategy $ withPostfix space pretty
            case instrules of
                [ i@IRule{} ] -> pretty i
                _ -> withLayout cfgLayoutDeriving flex vertical
      where
        flex = listAutoWrap Other "(" ")" "," instrules'
        vertical = listV Other "(" ")" "," instrules'
        instrules' = case instrules of
            [ IParen _ i ] -> [ i ]
            _ -> instrules

instance Pretty ConDecl where
    prettyPrint (ConDecl _ name types) = do
        pretty name
        unless (null types) $ do
            space
            withLayout cfgLayoutConDecl flex vertical
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
        withLayout cfgLayoutRecord flex vertical
      where
        flex = listH Declaration "{" "}" "," fielddecls
        vertical = listV Declaration "{" "}" "," fielddecls

instance Pretty FieldDecl where
    prettyPrint (FieldDecl _ names ty) = prettyTypesig Declaration names ty

instance Pretty QualConDecl where
    prettyPrint (QualConDecl _ mtyvarbinds mcontext condecl) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty condecl

instance Pretty GadtDecl where
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            withLayout cfgLayoutRecord (flex decls) (vertical decls)
            operator Type "->"
        pretty ty
      where
        flex = listH Declaration "{" "}" ","
        vertical = listV Declaration "{" "}" ","

instance Pretty Match where
    prettyPrint (Match _ name pats rhs mbinds) = do
        onside $ do
            pretty name
            unless (null pats) $ do
                space
                inter space $ map pretty pats
            pretty rhs
        mapM_ pretty mbinds

    prettyPrint (InfixMatch _ pat name pats rhs mbinds) = do
        onside $ do
            pretty pat
            pretty $ VarOp noNodeInfo name
            inter space $ map pretty pats
            pretty rhs
        mapM_ pretty mbinds

instance Pretty Rhs where
    prettyPrint (UnGuardedRhs _ expr) =
        cut . onside $ withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "="
            pretty expr
        vertical = do
            operatorV Declaration "="
            pretty expr

    prettyPrint (GuardedRhss _ guardedrhss) = withIndent cfgIndentMultiIf $ lined guardedrhss

instance Pretty GuardedRhs where
    prettyPrint (GuardedRhs _ stmts expr) =
        onside $ withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operatorSectionR Pattern "|" $ write "|"
            inter comma $ map pretty stmts
            operator Declaration "="
            pretty expr
        vertical = do
            operatorSectionR Pattern "|" $ write "|"
            inter comma $ map pretty stmts
            operatorV Declaration "="
            pretty expr

instance Pretty Context where
    prettyPrint (CxSingle _ asst) = do
        pretty asst
        operator Type "=>"

    prettyPrint (CxTuple _ assts) = do
        list Type "(" ")" "," assts
        operator Type "=>"

    prettyPrint (CxEmpty _) = do
        write "()"
        operator Type "=>"

instance Pretty FunDep where
    prettyPrint (FunDep _ names names') = do
        inter space $ map pretty names
        operator Declaration "->"
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

    prettyPrint (IParam _ ipname ty) = prettyTypesig Declaration [ ipname ] ty

    prettyPrint (EqualP _ ty ty') = do
        pretty ty
        operator Type "~"
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
        operator Type "->"
        pretty ty'

    prettyPrint (TyTuple _ boxed tys) = case boxed of
        Unboxed -> list Type "(#" "#)" "," tys
        Boxed -> list Type "(" ")" "," tys

    prettyPrint (TyUnboxedSum _ tys) = list Type "(#" "#)" "|" tys

    prettyPrint (TyList _ ty) = group Type "[" "]" $ pretty ty

    prettyPrint (TyParArray _ ty) = group Type "[:" ":]" $ pretty ty

    prettyPrint (TyApp _ ty ty') = do
        pretty ty
        space
        pretty ty'

    prettyPrint (TyVar _ name) = pretty name

    prettyPrint (TyCon _ qname) = pretty qname

    prettyPrint (TyParen _ ty) = parens $ pretty ty

    prettyPrint (TyInfix _ ty op ty') = do
        pretty ty
        withOperatorFormatting Type opname (prettyHSE op) id
        pretty ty'
      where
        opname = opName' $ case op of
            PromotedName _ qname -> qname
            UnpromotedName _ qname -> qname

    prettyPrint (TyKind _ ty kind) = do
        pretty ty
        operator Type "::"
        pretty kind

    prettyPrint t@(TyPromoted _ _promoted) = prettyHSE t

    prettyPrint (TyEquals _ ty ty') = do
        pretty ty
        operator Type  "~"
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
        operator Type "->"
        pretty kind'

    prettyPrint (KindParen _ kind) = parens $ pretty kind

    prettyPrint (KindVar _ qname) = pretty qname

    prettyPrint (KindApp _ kind kind') = do
        pretty kind
        space
        pretty kind'

    prettyPrint (KindTuple _ kinds) = list Type "'(" ")" "," kinds

    prettyPrint (KindList _ kind) = group Type "'[" "]" $ pretty kind

instance Pretty TyVarBind where
    prettyPrint (KindedVar _ name kind) = parens $ do
        pretty name
        operator Type "::"
        pretty kind

    prettyPrint (UnkindedVar _ name) = pretty name

instance Pretty TypeEqn where
    prettyPrint (TypeEqn _ ty ty') = do
        pretty ty
        operator Type "="
        pretty ty'

instance Pretty Exp where
    prettyPrint (Var _ qname) = pretty qname

    prettyPrint (OverloadedLabel _ str) = do
        write "#"
        string str

    prettyPrint (IPVar _ ipname) = pretty ipname

    prettyPrint (Con _ qname) = pretty qname

    prettyPrint (Lit _ literal) = pretty literal

    prettyPrint (e@(InfixApp _ _ qop _)) =
        prettyInfixApp opName Expression $ flattenInfix flattenInfixApp e
      where
        flattenInfixApp (InfixApp _ lhs qop' rhs) =
            if compareAST qop qop' == EQ then Just (lhs, qop', rhs) else Nothing
        flattenInfixApp _ = Nothing

    prettyPrint e@(App _ _ _) = case flattenApp flatten e of
        fn : args -> prettyApp fn args
        [] -> error "impossible"
      where
        flatten (App _ fn arg) = Just (fn, arg)
        flatten _ = Nothing

    prettyPrint (NegApp _ expr) = do
        write "-"
        pretty expr

    prettyPrint (Lambda _ pats expr) = do
        write "\\"
        space
        inter space $ map pretty pats
        operator Expression "->"
        pretty expr

    prettyPrint (Let _ binds expr) = withLayout cfgLayoutLet flex vertical
      where
        flex = aligned $ do
            write "let"
            withIndent cfgIndentLet $ pretty (CompactBinds binds)
            spaceOrNewline
            write "in "
            onside $ pretty expr
        vertical = aligned $ do
            write "let"
            withIndent cfgIndentLet $ pretty (CompactBinds binds)
            newline
            write "in "
            onside $ pretty expr

    prettyPrint (If _ expr expr' expr'') = withLayout cfgLayoutIf flex vertical
      where
        flex = do
            write "if "
            onside $ pretty expr
            spaceOrNewline
            write "then "
            onside $ pretty expr'
            spaceOrNewline
            write "else "
            onside $ pretty expr''
        vertical = do
            write "if "
            onside $ pretty expr
            withIndent cfgIndentIf $ do
                write "then "
                pretty expr'
                newline
                write "else "
                pretty expr''

    prettyPrint (MultiIf _ guardedrhss) = do
        write "if"
        withIndent cfgIndentMultiIf . lined $ map GuardedAlt guardedrhss

    prettyPrint (Case _ expr alts) = do
        write "case "
        pretty expr
        write " of"
        if null alts
            then write " { }"
            else withIndent cfgIndentCase $ lined alts

    prettyPrint (Do _ stmts) = do
        write "do"
        withIndent cfgIndentDo $ lined stmts

    prettyPrint (MDo _ stmts) = do
        write "mdo"
        withIndent cfgIndentDo $ lined stmts

    prettyPrint (Tuple _ boxed exprs) = case boxed of
        Boxed -> list Expression "(" ")" "," exprs
        Unboxed -> list Expression "(#" "#)" "," exprs

    prettyPrint (UnboxedSum _ before after expr) =
        group Expression "(#" "#)" . inter space $ replicate before (write "|") ++
            [ pretty expr ] ++ replicate after (write "|")

    prettyPrint (TupleSection _ boxed mexprs) = case boxed of
        Boxed -> list Expression "(" ")" "," $ map MayAst mexprs
        Unboxed -> list Expression "(#" "#)" "," $ map MayAst mexprs

    prettyPrint (List _ exprs) = list Expression "[" "]" "," exprs

    prettyPrint (ParArray _ exprs) = list Expression "[:" ":]" "," exprs

    prettyPrint (Paren _ expr) = parens $ pretty expr

    prettyPrint (LeftSection _ expr qop) = parens $ do
        pretty expr
        operatorSectionL Expression (opName qop) $ prettyHSE qop

    prettyPrint (RightSection _ qop expr) = parens $ do
        operatorSectionR Expression (opName qop) $ prettyHSE qop
        pretty expr

    prettyPrint (RecConstr _ qname fieldupdates) = do
        pretty qname
        sepSpace
        withLayout cfgLayoutRecord flex vertical
      where
        flex = listH Expression "{" "}" "," fieldupdates
        vertical = listV Expression "{" "}" "," fieldupdates

    prettyPrint (RecUpdate _ expr fieldupdates) = do
        pretty expr
        sepSpace
        withLayout cfgLayoutRecord flex vertical
      where
        flex = listH Expression "{" "}" "," fieldupdates
        vertical = listV Expression "{" "}" "," fieldupdates

    prettyPrint (EnumFrom _ expr) = group Expression "[" "]" $ do
        pretty expr
        operatorSectionL Expression ".." $ write ".."

    prettyPrint (EnumFromTo _ expr expr') = group Expression "[" "]" $ do
        pretty expr
        operator Expression ".."
        pretty expr'

    prettyPrint (EnumFromThen _ expr expr') = group Expression "[" "]" $ do
        pretty expr
        comma
        pretty expr'
        operatorSectionL Expression ".." $ write ".."

    prettyPrint (EnumFromThenTo _ expr expr' expr'') = group Expression "[" "]" $ do
        pretty expr
        comma
        pretty expr'
        operator Expression ".."
        pretty expr''

    prettyPrint (ParArrayFromTo _ expr expr') = group Expression "[:" ":]" $ do
        pretty expr
        operator Expression ".."
        pretty expr'

    prettyPrint (ParArrayFromThenTo _ expr expr' expr'') = group Expression "[:" ":]" $ do
        pretty expr
        comma
        pretty expr'
        operator Expression ".."
        pretty expr''

    prettyPrint (ListComp _ expr qualstmts) = group Expression "[" "]" $ do
        pretty expr
        list' Expression "|" "," qualstmts

    prettyPrint (ParComp _ expr qualstmtss) = group Expression "[" "]" $ do
        pretty expr
        aligned . inter newline . flip map qualstmtss $ \qualstmts ->
            cut $ list' Expression "|" "," qualstmts

    prettyPrint (ParArrayComp _ expr qualstmtss) = group Expression "[:" ":]" $ do
        pretty expr
        aligned . inter newline . flip map qualstmtss $ \qualstmts ->
            cut $ list' Expression "|" "," qualstmts

    prettyPrint (ExpTypeSig _ expr typ) = prettyTypesig Expression [ expr ] typ

    prettyPrint (VarQuote _ qname) = do
        write "'"
        pretty qname

    prettyPrint (TypQuote _ qname) = do
        write "''"
        pretty qname

    prettyPrint (BracketExp _ bracket) = pretty bracket

    prettyPrint (SpliceExp _ splice) = pretty splice

    prettyPrint (QuasiQuote _ str str') = do
        write "["
        string str
        write "|"
        string str'
        write "|]"

    prettyPrint (TypeApp _ typ) = do
        write "@"
        pretty typ

    prettyPrint (XTag _ xname xattrs mexpr exprs) = do
        write "<"
        pretty xname
        forM_ xattrs $ withPrefix space pretty
        mayM_ mexpr $ withPrefix space pretty
        write ">"
        mapM_ pretty exprs
        write "</"
        pretty xname
        write ">"

    prettyPrint (XETag _ xname xattrs mexpr) = do
        write "<"
        pretty xname
        forM_ xattrs $ withPrefix space pretty
        mayM_ mexpr $ withPrefix space pretty
        write "/>"

    prettyPrint (XPcdata _ str) = string str

    prettyPrint (XExpTag _ expr) = do
        write "<% "
        pretty expr
        write " %>"

    prettyPrint (XChildTag _ exprs) = do
        write "<%>"
        inter space $ map pretty exprs
        write "</%>"

    prettyPrint (CorePragma _ str expr) = do
        prettyPragma "CORE" . string $ show str
        space
        pretty expr

    prettyPrint (SCCPragma _ str expr) = do
        prettyPragma "SCC" . string $ show str
        space
        pretty expr

    prettyPrint (GenPragma _ str (a, b) (c, d) expr) = do
        prettyPragma "GENERATED" $ inter space
                                         [ string $ show str
                                         , int $ fromIntegral a
                                         , write ":"
                                         , int $ fromIntegral b
                                         , write "-"
                                         , int $ fromIntegral c
                                         , write ":"
                                         , int $ fromIntegral d
                                         ]
        space
        pretty expr

    prettyPrint (Proc _ pat expr) = do
        write "proc "
        pretty pat
        operator Expression "->"
        pretty expr

    prettyPrint (LeftArrApp _ expr expr') = do
        pretty expr
        operator Expression "-<"
        pretty expr'

    prettyPrint (RightArrApp _ expr expr') = do
        pretty expr
        operator Expression ">-"
        pretty expr'

    prettyPrint (LeftArrHighApp _ expr expr') = do
        pretty expr
        operator Expression "-<<"
        pretty expr'

    prettyPrint (RightArrHighApp _ expr expr') = do
        pretty expr
        operator Expression ">>-"
        pretty expr'

    prettyPrint (LCase _ alts) = do
        write "\\case"
        if null alts
            then write " { }"
            else withIndent cfgIndentCase $ lined alts

instance Pretty Alt where
    prettyPrint (Alt _ pat rhs mbinds) = do
        pretty pat
        pretty $ GuardedAlts rhs
        mapM_ pretty mbinds

instance Pretty XAttr where
    prettyPrint (XAttr _ xname expr) = do
        pretty xname
        operator Expression "="
        pretty expr

instance Pretty Pat where
    prettyPrint (PVar _ name) = pretty name

    prettyPrint (PLit _ sign literal) = do
        case sign of
            Signless _ -> return ()
            Negative _ -> write "-"
        pretty literal

    prettyPrint (PNPlusK _ name integer) = do
        pretty name
        operator Pattern "+"
        int integer

    prettyPrint (p@(PInfixApp _ _ qname _)) =
        prettyInfixApp opName Pattern $ flattenInfix flattenPInfixApp p
      where
        flattenPInfixApp (PInfixApp _ lhs qname' rhs) =
            if compareAST qname qname' == EQ
            then Just (lhs, QConOp noNodeInfo qname', rhs)
            else Nothing
        flattenPInfixApp _ = Nothing

    prettyPrint (PApp _ qname pats) = prettyApp qname pats

    prettyPrint (PTuple _ boxed pats) = case boxed of
        Boxed -> list Pattern "(" ")" "," pats
        Unboxed -> list Pattern "(#" "#)" "," pats

    prettyPrint (PUnboxedSum _ before after pat) =
        group Pattern "(#" "#)" . inter space $ replicate before (write "|") ++
            [ pretty pat ] ++ replicate after (write "|")

    prettyPrint (PList _ pats) = list Pattern "[" "]" "," pats

    prettyPrint (PParen _ pat) = parens $ pretty pat

    prettyPrint (PRec _ qname patfields) = do
        pretty qname
        sepSpace
        list Pattern "{" "}" "," patfields

    prettyPrint (PAsPat _ name pat) = do
        pretty name
        operator Pattern "@"
        pretty pat

    prettyPrint (PWildCard _) = write "_"

    prettyPrint (PIrrPat _ pat) = do
        write "~"
        pretty pat

    prettyPrint (PatTypeSig _ pat ty) = prettyTypesig Pattern [ pat ] ty

    prettyPrint (PViewPat _ expr pat) = do
        pretty expr
        operator Pattern "->"
        pretty pat

    prettyPrint (PRPat _ rpats) = list Pattern "[" "]" "," rpats

    prettyPrint (PXTag _ xname pxattrs mpat pats) = do
        write "<"
        pretty xname
        forM_ pxattrs $ withPrefix space pretty
        mayM_ mpat $ withPrefix space pretty
        write ">"
        mapM_ pretty pats
        write "<"
        pretty xname
        write ">"

    prettyPrint (PXETag _ xname pxattrs mpat) = do
        write "<"
        pretty xname
        forM_ pxattrs $ withPrefix space pretty
        mayM_ mpat $ withPrefix space pretty
        write "/>"

    prettyPrint (PXPcdata _ str) = string str

    prettyPrint (PXPatTag _ pat) = do
        write "<%"
        pretty pat
        write "%>"

    prettyPrint (PXRPats _ rpats) = do
        write "<["
        inter space $ map pretty rpats
        write "%>"

    prettyPrint (PSplice _ splice) = pretty splice

    prettyPrint (PQuasiQuote _ str str') = do
        write "[$"
        string str
        write "|"
        string str'
        write "|]"

    prettyPrint (PBangPat _ pat) = do
        write "!"
        pretty pat

instance Pretty PatField where
    prettyPrint (PFieldPat _ qname pat) = do
        pretty qname
        operator Pattern "="
        pretty pat

    prettyPrint (PFieldPun _ qname) = pretty qname

    prettyPrint (PFieldWildcard _) = write ".."

instance Pretty PXAttr where
    prettyPrint (PXAttr _ xname pat) = do
        pretty xname
        operator Pattern "="
        pretty pat

instance Pretty Literal where
    prettyPrint (Char _ _ str) = do
        write "'"
        string str
        write "'"

    prettyPrint (String _ _ str) = do
        write "\""
        string str
        write "\""

    prettyPrint (Int _ _ str) = string str

    prettyPrint (Frac _ _ str) = string str

    prettyPrint (PrimInt _ _ str) = do
        string str
        write "#"

    prettyPrint (PrimWord _ _ str) = do
        string str
        write "##"

    prettyPrint (PrimFloat _ _ str) = do
        string str
        write "#"

    prettyPrint (PrimDouble _ _ str) = do
        string str
        write "##"

    prettyPrint (PrimChar _ _ str) = do
        write "'"
        string str
        write "'#"

    prettyPrint (PrimString _ _ str) = do
        write "\""
        string str
        write "\"#"

instance Pretty QualStmt where
    prettyPrint (QualStmt _ stmt) = pretty stmt

    prettyPrint (ThenTrans _ expr) = do
        write "then "
        pretty expr

    prettyPrint (ThenBy _ expr expr') = do
        write "then "
        pretty expr
        write " by "
        pretty expr'

    prettyPrint (GroupBy _ expr) = do
        write "then group by "
        pretty expr

    prettyPrint (GroupUsing _ expr) = do
        write "then group using "
        pretty expr

    prettyPrint (GroupByUsing _ expr expr') = do
        write "then group by "
        pretty expr
        write " using "
        pretty expr'

instance Pretty Stmt where
    prettyPrint (Generator _ pat expr) = do
        pretty pat
        onside $ do
            operator Expression "<-"
            pretty expr

    prettyPrint (Qualifier _ expr) = onside $ pretty expr

    prettyPrint (LetStmt _ binds) = do
        write "let "
        pretty $ CompactBinds binds

    prettyPrint (RecStmt _ stmts) = do
        write "rec "
        aligned $ lined stmts

instance Pretty FieldUpdate where
    prettyPrint (FieldUpdate _ qname expr) = do
        pretty qname
        operator Expression "="
        pretty expr

    prettyPrint (FieldPun _ qname) = pretty qname

    prettyPrint (FieldWildcard _) = write ".."

instance Pretty QOp where
    prettyPrint qop = withOperatorFormatting Expression (opName qop) (prettyHSE qop) id

instance Pretty Op where
    prettyPrint (VarOp l name) = prettyPrint (QVarOp l (UnQual noNodeInfo name))
    prettyPrint (ConOp l name) = prettyPrint (QConOp l (UnQual noNodeInfo name))

instance Pretty Bracket where
    prettyPrint (ExpBracket _ expr) = group Expression "[|" "|]" $ pretty expr

    prettyPrint (PatBracket _ pat) = group Expression "[p|" "|]" $ pretty pat

    prettyPrint (TypeBracket _ ty) = group Expression "[t|" "|]" $ pretty ty

    prettyPrint (DeclBracket _ decls) = group Expression "[d|" "|]" . aligned $ lined decls

instance Pretty Splice where
    prettyPrint (IdSplice _ str) = do
        write "$"
        string str

    prettyPrint (ParenSplice _ expr) = group Expression "$(" ")" $ pretty expr

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
        operator Expression "="
        pretty expr'

instance Pretty RuleVar where
    prettyPrint (RuleVar _ name) = pretty name

    prettyPrint (TypedRuleVar _ name ty) =
        parens $ prettyTypesig Declaration [ name ] ty

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
        inter (operator Expression "|") $ map pretty booleanformulas

    prettyPrint (ParenFormula _ booleanformula) = parens $ pretty booleanformula

-- Stick with HSE
instance Pretty DerivStrategy

instance Pretty DataOrNew

instance Pretty BangType

instance Pretty Unpackedness

instance Pretty RPat

instance Pretty ModuleName

instance Pretty QName

instance Pretty Name

instance Pretty IPName

instance Pretty XName

instance Pretty Safety

instance Pretty CallConv

instance Pretty Overlap

-- Helpers
newtype GuardedAlt l = GuardedAlt (GuardedRhs l)
    deriving (Functor, Annotated)

instance Pretty GuardedAlt where
    prettyPrint (GuardedAlt (GuardedRhs _ stmts expr)) = cut $ do
        onside $ do
            operatorSectionR Pattern "|" $ write "|"
            inter comma $ map pretty stmts
            operator Expression "->"
            pretty expr

newtype GuardedAlts l = GuardedAlts (Rhs l)
    deriving (Functor, Annotated)

instance Pretty GuardedAlts where
    prettyPrint (GuardedAlts (UnGuardedRhs _ expr)) = cut . onside $ do
        operator Expression "->"
        pretty expr

    prettyPrint (GuardedAlts (GuardedRhss _ guardedrhss)) =
        aligned . lined $ map GuardedAlt guardedrhss

newtype CompactBinds l = CompactBinds (Binds l)
    deriving (Functor, Annotated)

instance Pretty CompactBinds where
    prettyPrint (CompactBinds (BDecls _ decls)) = aligned $ lined decls
    prettyPrint (CompactBinds (IPBinds _ ipbinds)) = aligned $ lined ipbinds

newtype MayAst a l = MayAst (Maybe (a l))

instance Functor a => Functor (MayAst a) where
    fmap _ (MayAst Nothing) = MayAst Nothing
    fmap f (MayAst (Just x)) = MayAst . Just $ fmap f x

instance Annotated a => Annotated (MayAst a) where
    ann (MayAst Nothing) = undefined
    ann (MayAst (Just x)) = ann x
    amap _ (MayAst Nothing) = MayAst Nothing
    amap f (MayAst (Just x)) = MayAst . Just $ amap f x

instance (Annotated a, Pretty a) => Pretty (MayAst a) where
    prettyPrint (MayAst x) = mapM_ pretty x

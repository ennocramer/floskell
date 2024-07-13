{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Floskell.Pretty ( Pretty(..), pretty ) where

import           Control.Applicative          ( (<|>) )
import           Control.Monad
                 ( forM_, guard, replicateM_, unless, void, when )
import           Control.Monad.State.Strict   ( get, gets, modify )

import           Data.Bool                    ( bool )
import           Data.List                    ( groupBy, sortBy, sortOn )
import           Data.Maybe                   ( catMaybes, fromMaybe )
import qualified Data.Set                     as Set
import           Data.Text                    ( Text )
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL

import qualified Floskell.Buffer              as Buffer
import           Floskell.Config
import           Floskell.Imports
                 ( groupImports, sortImports, splitImports )
import           Floskell.Printers
import           Floskell.Types

import qualified Language.Haskell.Exts.Pretty as HSE
import           Language.Haskell.Exts.Syntax

-- | Like `span`, but comparing adjacent items.
run :: (a -> a -> Bool) -> [a] -> ([a], [a])
run _ [] = ([], [])
run _ [ x ] = ([ x ], [])
run eq (x : y : xs)
    | eq x y = let (ys, zs) = run eq (y : xs) in (x : ys, zs)
    | otherwise = ([ x ], y : xs)

-- | Like `groupBy`, but comparing adjacent items.
runs :: (a -> a -> Bool) -> [a] -> [[a]]
runs _ [] = []
runs eq xs = let (ys, zs) = run eq xs in ys : runs eq zs

stopImportModule :: TabStop
stopImportModule = TabStop "import-module"

stopImportSpec :: TabStop
stopImportSpec = TabStop "import-spec"

stopRecordField :: TabStop
stopRecordField = TabStop "record"

stopRhs :: TabStop
stopRhs = TabStop "rhs"

flattenApp :: Annotated ast
           => (ast NodeInfo -> Maybe (ast NodeInfo, ast NodeInfo))
           -> ast NodeInfo
           -> [ast NodeInfo]
flattenApp fn = go . amap (\info -> info { nodeInfoLeadingComments  = []
                                         , nodeInfoTrailingComments = []
                                         })
  where
    go x = case fn x of
        Just (lhs, rhs) -> let lhs' = go $ copyComments Before x lhs
                               rhs' = go $ copyComments After x rhs
                           in
                               lhs' ++ rhs'
        Nothing -> [ x ]

flattenInfix
    :: (Annotated ast1, Annotated ast2)
    => (ast1 NodeInfo -> Maybe (ast1 NodeInfo, ast2 NodeInfo, ast1 NodeInfo))
    -> ast1 NodeInfo
    -> (ast1 NodeInfo, [(ast2 NodeInfo, ast1 NodeInfo)])
flattenInfix fn = go . amap (\info -> info { nodeInfoLeadingComments  = []
                                           , nodeInfoTrailingComments = []
                                           })
  where
    go x = case fn x of
        Just (lhs, op, rhs) ->
            let (lhs', ops) = go $ copyComments Before x lhs
                (lhs'', ops') = go $ copyComments After x rhs
            in
                (lhs', ops ++ (op, lhs'') : ops')
        Nothing -> (x, [])

-- | Pretty printing prettyHSE using haskell-src-exts pretty printer
prettyHSE :: HSE.Pretty (ast NodeInfo) => ast NodeInfo -> Printer ()
prettyHSE ast = string $ HSE.prettyPrint ast

-- | Type class for pretty-printable types.
class Pretty ast where
    prettyPrint :: ast NodeInfo -> Printer ()
    default prettyPrint
        :: HSE.Pretty (ast NodeInfo) => ast NodeInfo -> Printer ()
    prettyPrint = prettyHSE

-- | Pretty print a syntax tree with annotated comments
pretty :: (Annotated ast, Pretty ast) => ast NodeInfo -> Printer ()
pretty ast = do
    printCommentsBefore True ast
    prettyPrint ast
    printCommentsAfter ast

prettyOnside :: (Annotated ast, Pretty ast) => ast NodeInfo -> Printer ()
prettyOnside ast = do
    closeEolComment
    nl <- gets psNewline
    if nl
        then do
            printCommentsBefore True ast
            onside $ cut $ prettyPrint ast
            printCommentsAfter ast
        else onside $ pretty ast

-- | Compare two AST nodes ignoring the annotation
compareAST
    :: (Functor ast, Ord (ast ())) => ast NodeInfo -> ast NodeInfo -> Ordering
compareAST a b = void a `compare` void b

-- | Return leading comments.
filterComments :: Annotated a => Location -> a NodeInfo -> [Comment]
filterComments Before = nodeInfoLeadingComments . ann
filterComments After = nodeInfoTrailingComments . ann

-- | Copy comments from one AST node to another.
copyComments :: (Annotated ast1, Annotated ast2)
             => Location
             -> ast1 NodeInfo
             -> ast2 NodeInfo
             -> ast2 NodeInfo
copyComments Before from to =
    amap (\n ->
          n { nodeInfoLeadingComments = nodeInfoLeadingComments $ ann from })
         to
copyComments After from to =
    amap (\n ->
          n { nodeInfoTrailingComments = nodeInfoTrailingComments $ ann from })
         to

-- | Pretty print a comment.
printComment :: Int -> Comment -> Printer ()
printComment correction Comment{..} = do
    col <- getNextColumn
    let padding = max 0 $ srcSpanStartColumn commentSpan + correction - col - 1
    case commentType of
        PreprocessorDirective -> do
            ensureNewline
            column 0 $ string commentText
            modify (\s -> s { psEolComment = True })
        InlineComment -> do
            write $ T.replicate padding " "
            write "{-"
            string commentText
            write "-}"
        LineComment -> do
            write $ T.replicate padding " "
            write "--"
            string commentText
            modify (\s -> s { psEolComment = True })
        IgnoredLine -> do
            ensureNewline
            column 0 $ string commentText
            modify (\s -> s { psEolComment = True })

printCommentsBefore :: Annotated ast => Bool -> ast NodeInfo -> Printer ()
printCommentsBefore nlBefore ast = unless (null comments) $ suppressOnside $ do
    when nlBefore ensureNewline
    col <- getNextColumn
    printCommentsInternal (col - srcSpanStartColumn (nodeSpan ast) + 1)
                          comments
    let distance = srcSpanStartLine (nodeSpan ast)
            - srcSpanEndLine (commentSpan (last comments))
    when (distance > 0) $ do
        ensureNewline
        replicateM_ (distance - 1) newline
  where
    comments = nodeInfoLeadingComments $ ann ast

printCommentsAfter :: Annotated ast => ast NodeInfo -> Printer ()
printCommentsAfter ast = unless (null comments) $ suppressOnside $ do
    let distance = srcSpanStartLine (commentSpan (head comments))
            - srcSpanEndLine (nodeSpan ast)
    when (distance > 0) $ do
        ensureNewline
        replicateM_ (distance - 1) newline
    col <- getNextColumn
    printCommentsInternal (col - srcSpanEndColumn (nodeSpan ast) + 1) comments
  where
    comments = nodeInfoTrailingComments $ ann ast

printCommentsInternal :: Int -> [Comment] -> Printer ()
printCommentsInternal correction comments = do
    printComment correction (head comments)
    forM_ (zip (tail comments) (map (srcSpanEndLine . commentSpan) comments)) $
        \(comment, prevLine) -> do
            let nextLine = srcSpanStartLine $ commentSpan comment
            replicateM_ (nextLine - prevLine) newline
            printComment correction comment

-- | Return the configuration name of an operator
opName :: QOp a -> Text
opName op = case op of
    (QVarOp _ qname) -> opName' qname
    (QConOp _ qname) -> opName' qname

-- | Return the configuration name of an operator
opName' :: QName a -> Text
opName' (Qual _ _ name) = opName'' name
opName' (UnQual _ name) = opName'' name
opName' (Special _ (FunCon _)) = "->"
opName' (Special _ (Cons _)) = ":"
opName' (Special _ _) = ""

-- | Return the configuration name of an operator
opName'' :: Name a -> Text
opName'' (Ident _ _) = "``"
opName'' (Symbol _ str) = T.pack str

lineDelta :: Annotated ast => ast NodeInfo -> ast NodeInfo -> Int
lineDelta prev next = nextLine - prevLine
  where
    prevLine = maximum (prevNodeLine : prevCommentLines)

    nextLine = minimum (nextNodeLine : nextCommentLines)

    prevNodeLine = srcSpanEndLine $ nodeSpan prev

    nextNodeLine = srcSpanStartLine $ nodeSpan next

    prevCommentLines = map (srcSpanEndLine . commentSpan) $
        filterComments After prev

    nextCommentLines = map (srcSpanStartLine . commentSpan) $
        filterComments Before next

linedFn :: Annotated ast
        => (ast NodeInfo -> Printer ())
        -> [ast NodeInfo]
        -> Printer ()
linedFn fn xs = do
    preserveP <- getOption cfgOptionPreserveVerticalSpace
    if preserveP
        then case xs of
            x : xs' -> do
                cut $ fn x
                forM_ (zip xs xs') $ \(prev, cur) -> do
                    replicateM_ (min 2 (max 1 $ lineDelta prev cur)) newline
                    cut $ fn cur
            [] -> return ()
        else inter newline $ map (cut . fn) xs

lined :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
lined = linedFn pretty

linedOnside :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
linedOnside = linedFn prettyOnside

listVOpLen :: LayoutContext -> Text -> Printer Int
listVOpLen ctx sep = do
    ws <- getConfig (cfgOpWs ctx sep . cfgOp)
    return $ if wsLinebreak After ws
             then 0
             else T.length sep + if wsSpace After ws then 1 else 0

listVinternal :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> Text
              -> [ast NodeInfo]
              -> Printer ()
listVinternal ctx sep xs = case xs of
    [] -> newline
    (x : xs') -> do
        nl <- gets psNewline
        col <- getNextColumn
        delta <- listVOpLen ctx sep
        let itemCol = if nl && length xs > 1 then col + delta else col
            sepCol = itemCol - delta
        column itemCol $ do
            printCommentsBefore False x
            cut $ prettyPrint x
            printCommentsAfter x
        -- `column sepCol` must not be within `column itemCol`, or the
        -- former can suppress onside for the latter.
        forM_ xs' $ \x' -> do
            column itemCol $ printCommentsBefore True x'
            column sepCol $ operatorV ctx sep
            column itemCol $ cut $ prettyPrint x'
            column itemCol $ printCommentsAfter x'

listH :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> Text
      -> Text
      -> Text
      -> [ast NodeInfo]
      -> Printer ()
listH _ open close _ [] = do
    write open
    write close

listH ctx open close sep xs =
    groupH ctx open close . inter (operatorH ctx sep) $ map pretty xs

listV :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> Text
      -> Text
      -> Text
      -> [ast NodeInfo]
      -> Printer ()
listV ctx open close sep xs = groupV ctx open close $ do
    ws <- getConfig (cfgOpWs ctx sep . cfgOp)
    ws' <- getConfig (cfgGroupWs ctx open . cfgGroup)
    unless (wsLinebreak Before ws' || wsSpace After ws' || wsLinebreak After ws
            || not (wsSpace After ws))
           space
    listVinternal ctx sep xs

list :: (Annotated ast, Pretty ast)
     => LayoutContext
     -> Text
     -> Text
     -> Text
     -> [ast NodeInfo]
     -> Printer ()
list ctx open close sep xs = oneline hor <|> ver
  where
    hor = listH ctx open close sep xs

    ver = listV ctx open close sep xs

listH' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> Text
       -> [ast NodeInfo]
       -> Printer ()
listH' ctx sep = inter (operatorH ctx sep) . map pretty

listV' :: (Annotated ast, Pretty ast)
       => LayoutContext
       -> Text
       -> [ast NodeInfo]
       -> Printer ()
listV' ctx sep xs =
    if length xs > 1 then listVinternal ctx sep xs else mapM_ pretty xs

list' :: (Annotated ast, Pretty ast)
      => LayoutContext
      -> Text
      -> [ast NodeInfo]
      -> Printer ()
list' ctx sep xs = oneline hor <|> ver
  where
    hor = listH' ctx sep xs

    ver = listV' ctx sep xs

listAutoWrap :: (Annotated ast, Pretty ast)
             => LayoutContext
             -> Text
             -> Text
             -> Text
             -> [ast NodeInfo]
             -> Printer ()
listAutoWrap _ open close _ [] = do
    write open
    write close

listAutoWrap ctx open close sep xs =
    aligned . groupH ctx open close $ listAutoWrap' ctx sep xs

listAutoWrap' :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> Text
              -> [ast NodeInfo]
              -> Printer ()
listAutoWrap' _ _ [] = return ()
listAutoWrap' ctx sep (x : xs) = aligned $ do
    ws <- getConfig (cfgOpWs ctx sep . cfgOp)
    let correction = if wsLinebreak After ws
                     then 0
                     else T.length sep + if wsSpace After ws then 1 else 0
    col <- getNextColumn
    pretty x
    go (col - correction) xs
  where
    go _ [] = return ()
    go col [ x' ] = do
        printCommentsBefore True x'
        column col $ operator ctx sep
        prettyPrint x'
        printCommentsAfter x'
    go col (x' : xs') = do
        printCommentsBefore True x'
        cut $ do
            column col $ operator ctx sep
            prettyPrint x'
            printCommentsAfter x'
        go col xs'

measure :: Printer a -> Printer (Maybe Int)
measure p = do
    s <- get
    let s' = s { psBuffer = Buffer.empty, psEolComment = False }
    return $ case execPrinter (oneline p) s' of
        Nothing -> Nothing
        Just (_, s'') -> Just . (\x -> x - psIndentLevel s) . fromIntegral
            . TL.length . Buffer.toLazyText $ psBuffer s''

measure' :: Printer a -> Printer (Maybe [Int])
measure' p = fmap (: []) <$> measure p

measureMatch :: Match NodeInfo -> Printer (Maybe [Int])
measureMatch (Match _ name pats rhs _) = case rhs of
    UnGuardedRhs _ _ -> measure' (prettyApp name pats)
    GuardedRhss _ grhss -> fmap sequence (mapM measureGuardedRhs grhss)
measureMatch (InfixMatch _ pat name pats _ Nothing) = measure' go
  where
    go = do
        pretty pat
        withOperatorFormatting Pattern
                               (opName'' name)
                               (prettyHSE $ VarOp noNodeInfo name)
                               id
        inter spaceOrNewline $ map pretty pats
measureMatch _ = return Nothing

measureGuardedRhs :: GuardedRhs NodeInfo -> Printer (Maybe Int)
measureGuardedRhs (GuardedRhs _ stmts _) = measure $
    withIndentConfig cfgIndentMultiIf (space >> aligned p) (flip indented p)
  where
    p = prettyGuard stmts

measureDecl :: Decl NodeInfo -> Printer (Maybe [Int])
measureDecl (PatBind _ pat _ Nothing) = measure' (pretty pat)
measureDecl (FunBind _ matches) =
    fmap concat . sequence <$> traverse measureMatch matches
measureDecl _ = return Nothing

measureClassDecl :: ClassDecl NodeInfo -> Printer (Maybe [Int])
measureClassDecl (ClsDecl _ decl) = measureDecl decl
measureClassDecl _ = return Nothing

measureInstDecl :: InstDecl NodeInfo -> Printer (Maybe [Int])
measureInstDecl (InsDecl _ decl) = measureDecl decl
measureInstDecl _ = return Nothing

measureAlt :: Alt NodeInfo -> Printer (Maybe [Int])
measureAlt (Alt _ pat _ Nothing) = measure' (pretty pat)
measureAlt _ = return Nothing

withComputedTabStop :: TabStop
                    -> (AlignConfig -> Bool)
                    -> (a -> Printer (Maybe [Int]))
                    -> [a]
                    -> Printer b
                    -> Printer b
withComputedTabStop name predicate fn xs p = do
    enabled <- getConfig (predicate . cfgAlign)
    (limAbs, limRel) <- getConfig (cfgAlignLimits . cfgAlign)
    mtabss <- sequence <$> traverse fn xs
    let tab = do
            tabss <- mtabss
            let tabs = concat tabss
                maxtab = maximum tabs
                mintab = minimum tabs
                delta = maxtab - mintab
                diff = delta * 100 `div` maxtab
            guard enabled
            guard $ delta <= limAbs || diff <= limRel
            return maxtab
    withTabStops [ (name, tab) ] p

------------------------------------------------------------------------
-- Module
-- | Extract the name as a String from a ModuleName
moduleName :: ModuleName a -> String
moduleName (ModuleName _ s) = s

prettyPragmas :: [ModulePragma NodeInfo] -> Printer ()
prettyPragmas ps = do
    splitP <- getOption cfgOptionSplitLanguagePragmas
    sortP <- getOption cfgOptionSortPragmas
    let ps' = if splitP then concatMap splitPragma ps else ps
    let ps'' = if sortP then sortBy compareAST ps' else ps'
    inter blankline . map lined $ groupBy sameType ps''
  where
    splitPragma (LanguagePragma anno langs) =
        map (LanguagePragma anno . (: [])) langs
    splitPragma p = [ p ]

    sameType LanguagePragma{} LanguagePragma{} = True
    sameType OptionsPragma{} OptionsPragma{} = True
    sameType AnnModulePragma{} AnnModulePragma{} = True
    sameType _ _ = False

prettyImports :: [ImportDecl NodeInfo] -> Printer ()
prettyImports is = do
    sortP <- getOption cfgOptionSortImports
    alignModuleP <- getConfig (cfgAlignImportModule . cfgAlign)
    alignSpecP <- getConfig (cfgAlignImportSpec . cfgAlign)
    let maxNameLength = maximum $ map (length . moduleName . importModule) is
        alignModule = if alignModuleP then Just 16 else Nothing
        alignSpec = if alignSpecP
                    then Just (fromMaybe 0 alignModule + 1 + maxNameLength)
                    else Nothing
    withTabStops [ (stopImportModule, alignModule)
                 , (stopImportSpec, alignSpec)
                 ] $ case sortP of
        NoImportSort -> lined is
        SortImportsByPrefix -> prettyGroups . groupImports 0 $ sortImports is
        SortImportsByGroups groups -> prettyGroups $ splitImports groups is
  where
    prettyGroups = inter blankline . map (inter newline . map (cut . pretty))

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
skipBlankDecl a b = case (a, b) of
    (PatBind{}, PatBind{}) -> True
    _ -> skipBlankAfterDecl a

skipBlankClassDecl :: ClassDecl NodeInfo -> ClassDecl NodeInfo -> Bool
skipBlankClassDecl a _ = case a of
    (ClsDecl _ decl) -> skipBlankAfterDecl decl
    ClsTyDef{} -> True
    ClsDefSig{} -> True
    _ -> False

skipBlankInstDecl :: InstDecl NodeInfo -> InstDecl NodeInfo -> Bool
skipBlankInstDecl a _ = case a of
    (InsDecl _ decl) -> skipBlankAfterDecl decl
    _ -> False

prettyDecls :: (Annotated ast, Pretty ast)
            => (ast NodeInfo -> ast NodeInfo -> Bool)
            -> DeclarationConstruct
            -> [ast NodeInfo]
            -> Printer ()
prettyDecls fn dc = inter sep . map lined . runs fn
  where
    sep = bool blankline newline . Set.member dc
        =<< getOption cfgOptionDeclNoBlankLines

prettySimpleDecl :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
                 => ast1 NodeInfo
                 -> Text
                 -> ast2 NodeInfo
                 -> Printer ()
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

prettyConDecls :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
prettyConDecls condecls = do
    alignedConDecls <- getOption cfgOptionAlignSumTypeDecl
    if alignedConDecls && length condecls > 1
        then withLayout cfgLayoutDeclaration flex' vertical'
        else withLayout cfgLayoutDeclaration flex vertical
  where
    flex = do
        operator Declaration "="
        withLayout cfgLayoutConDecls flexDecls verticalDecls

    flex' = withLayout cfgLayoutConDecls flexDecls' verticalDecls'

    vertical = do
        operatorV Declaration "="
        withLayout cfgLayoutConDecls flexDecls verticalDecls

    vertical' = withLayout cfgLayoutConDecls flexDecls' verticalDecls'

    flexDecls = listAutoWrap' Declaration "|" condecls

    flexDecls' = horizontalDecls' <|> verticalDecls'

    horizontalDecls' = do
        operatorH Declaration "="
        listH' Declaration "|" condecls

    verticalDecls = listV' Declaration "|" condecls

    verticalDecls' = do
        withOperatorFormattingV Declaration "|" (write "=") id
        listV' Declaration "|" condecls

prettyForall :: (Annotated ast, Pretty ast) => [ast NodeInfo] -> Printer ()
prettyForall vars = do
    write "forall "
    inter space $ map pretty vars
    operator Type "."

prettyTypesig :: (Annotated ast, Pretty ast)
              => LayoutContext
              -> [ast NodeInfo]
              -> Type NodeInfo
              -> Printer ()
prettyTypesig ctx names ty = do
    listAutoWrap' ctx "," names
    atTabStop stopRecordField
    withIndentConfig cfgIndentTypesig align indentby
  where
    align = onside . alignOnOperator ctx "::" $ pretty ty

    indentby i = indented i $ do
        operator ctx "::"
        nl <- gets psNewline
        when nl $ do
            delta <- listVOpLen ctx "->"
            write $ T.replicate delta " "
        pretty ty

prettyApp :: (Annotated ast1, Annotated ast2, Pretty ast1, Pretty ast2)
          => ast1 NodeInfo
          -> [ast2 NodeInfo]
          -> Printer ()
prettyApp fn args = withLayout cfgLayoutApp flex vertical
  where
    flex = do
        pretty fn
        forM_ args $ \arg -> cut $ do
            spaceOrNewline
            pretty arg

    vertical = do
        pretty fn
        withIndent cfgIndentApp $ lined args

prettyInfixApp
    :: (Annotated ast, Pretty ast, Annotated op, HSE.Pretty (op NodeInfo))
    => (op NodeInfo -> Text)
    -> LayoutContext
    -> (ast NodeInfo, [(op NodeInfo, ast NodeInfo)])
    -> Printer ()
prettyInfixApp nameFn ctx (lhs, args) =
    withLayout cfgLayoutInfixApp flex vertical
  where
    flex = do
        pretty lhs
        forM_ args $ \(op, arg) -> cut $ do
            withOperatorFormatting ctx (nameFn op) (prettyOp op) id
            pretty arg

    vertical = do
        pretty lhs
        forM_ args $ \(op, arg) -> do
            withOperatorFormattingV ctx (nameFn op) (prettyOp op) id
            pretty arg

    prettyOp op = do
        printCommentsBefore True op
        prettyHSE op
        printCommentsAfter op

prettyRecord :: (Annotated ast1, Pretty ast1, Annotated ast2, Pretty ast2)
             => (ast2 NodeInfo -> Printer (Maybe Int))
             -> LayoutContext
             -> ast1 NodeInfo
             -> [ast2 NodeInfo]
             -> Printer ()
prettyRecord len ctx name fields = withLayout cfgLayoutRecord flex vertical
  where
    flex = do
        withOperatorFormattingH ctx "record" (pretty name) id
        prettyRecordFields len ctx fields

    vertical = do
        withOperatorFormatting ctx "record" (pretty name) id
        prettyRecordFields len ctx fields

prettyRecordFields :: (Annotated ast, Pretty ast)
                   => (ast NodeInfo -> Printer (Maybe Int))
                   -> LayoutContext
                   -> [ast NodeInfo]
                   -> Printer ()
prettyRecordFields len ctx fields = withLayout cfgLayoutRecord flex vertical
  where
    flex = groupH ctx "{" "}" $ listAutoWrap' ctx "," fields

    vertical = groupV ctx "{" "}" $
        withComputedTabStop stopRecordField
                            cfgAlignRecordFields
                            (fmap (fmap pure) . len)
                            fields $ listVinternal ctx "," fields

prettyPragma :: Text -> Printer () -> Printer ()
prettyPragma name = prettyPragma' name . Just

prettyPragma' :: Text -> Maybe (Printer ()) -> Printer ()
prettyPragma' name mp = do
    write "{-# "
    write name
    mayM_ mp $ withPrefix space aligned
    write " #-}"

prettyBinds :: Binds NodeInfo -> Printer ()
prettyBinds binds = withIndentBy cfgIndentWhere $ do
    write "where"
    withIndent cfgIndentWhereBinds $ pretty binds

prettyGuard :: [Stmt NodeInfo] -> Printer ()
prettyGuard stmts = do
    operatorSectionR Pattern "|" $ write "|"
    withLayout cfgLayoutDeclaration flex vertical
  where
    flex = listAutoWrap' pat sep stmts
    vertical = list' pat sep stmts
    pat = Pattern
    sep = ","

instance Pretty Module where
    prettyPrint (Module _ mhead pragmas imports decls) = inter blankline $
        catMaybes [ ifNotEmpty prettyPragmas pragmas
                  , pretty <$> mhead
                  , ifNotEmpty prettyImports imports
                  , ifNotEmpty (prettyDecls skipBlankDecl DeclModule) decls
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
        withLayout cfgLayoutExportSpecList flex vertical
      where
        flex = do
            mayM_ mexports $ \(ExportSpecList _ exports) -> do
                space
                listAutoWrap Other "(" ")" "," exports
            write " where"

        vertical = do
            mayM_ mexports $ \(ExportSpecList _ exports) -> do
                withIndent cfgIndentExportSpecList $
                    listV Other "(" ")" "," exports
            write " where"

instance Pretty WarningText where
    prettyPrint (DeprText _ s) = write "{-# DEPRECATED " >> string (show s)
        >> write " #-}"
    prettyPrint (WarnText _ s) = write "{-# WARNING " >> string (show s)
        >> write " #-}"

instance Pretty ExportSpec

instance Pretty ImportDecl where
    prettyPrint ImportDecl{..} = do
        inter space . map string $
            filter (not . null)
                   [ "import"
                   , if importSrc then "{-# SOURCE #-}" else ""
                   , if importSafe then "safe" else ""
                   , if importQualified then "qualified" else ""
                   , maybe "" show importPkg
                   ]
        atTabStop stopImportModule
        space
        string $ moduleName importModule
        mayM_ importAs $ \name -> do
            atTabStop stopImportSpec
            write " as "
            pretty name
        mayM_ importSpecs pretty

instance Pretty ImportSpecList where
    prettyPrint (ImportSpecList _ hiding specs) = do
        sortP <- getOption cfgOptionSortImportLists
        let specs' = if sortP then sortOn HSE.prettyPrint specs else specs
        atTabStop stopImportSpec
        withLayout cfgLayoutImportSpecList (flex specs') (vertical specs')
      where
        flex imports = withIndentFlex cfgIndentImportSpecList $ do
            when hiding $ write "hiding "
            listAutoWrap Other "(" ")" "," imports

        vertical imports = withIndent cfgIndentImportSpecList $ do
            when hiding $ write "hiding "
            listV Other "(" ")" "," imports

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

    prettyPrint (ClosedTypeFamDecl _
                                   declhead
                                   mresultsig
                                   minjectivityinfo
                                   typeeqns) = depend "type family" $ do
        pretty declhead
        mayM_ mresultsig pretty
        mayM_ minjectivityinfo pretty
        write " where"
        newline
        linedOnside typeeqns

    prettyPrint (DataDecl _ dataornew mcontext declhead qualcondecls derivings) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null qualcondecls) $ prettyConDecls qualcondecls
        mapM_ pretty derivings

    prettyPrint (GDataDecl _
                           dataornew
                           mcontext
                           declhead
                           mkind
                           gadtdecls
                           derivings) = do
        depend' (pretty dataornew) $ do
            mapM_ pretty mcontext
            pretty declhead
            mayM_ mkind $ \kind -> do
                operator Declaration "::"
                pretty kind
            write " where"
            newline
            linedOnside gadtdecls
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
            prettyConDecls qualcondecls
        mapM_ pretty derivings

    prettyPrint (GDataInsDecl _ dataornew ty mkind gadtdecls derivings) = do
        depend' (pretty dataornew >> write " instance") $ do
            pretty ty
            mayM_ mkind $ \kind -> do
                operator Declaration "::"
                pretty kind
            write " where"
            newline
            linedOnside gadtdecls
        mapM_ pretty derivings

    prettyPrint (ClassDecl _ mcontext declhead fundeps mclassdecls) = do
        depend "class" $ do
            mapM_ pretty mcontext
            pretty declhead
            unless (null fundeps) $ do
                operator Declaration "|"
                list' Declaration "," fundeps
        mayM_ mclassdecls $ \decls -> do
            write " where"
            withIndent cfgIndentClass $ withComputedTabStop stopRhs
                                                            cfgAlignClass
                                                            measureClassDecl
                                                            decls $
                prettyDecls skipBlankClassDecl DeclClass decls

    prettyPrint (InstDecl _ moverlap instrule minstdecls) = do
        depend "instance" $ do
            mapM_ pretty moverlap
            pretty instrule
        mayM_ minstdecls $ \decls -> do
            write " where"
            withIndent cfgIndentClass $
                withComputedTabStop stopRhs cfgAlignClass measureInstDecl decls $
                prettyDecls skipBlankInstDecl DeclInstance decls

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (DerivDecl _ mderivstrategy moverlap instrule) =
        depend "deriving" $ do
            mayM_ mderivstrategy $ withPostfix space pretty
            write "instance "
            mayM_ moverlap $ withPostfix space pretty
            pretty instrule
#else
    prettyPrint (DerivDecl _ moverlap instrule) = depend "deriving" $ do
        write "instance "
        mayM_ moverlap $ withPostfix space pretty
        pretty instrule
#endif

    prettyPrint (InfixDecl _ assoc mint ops) = onside $ do
        pretty assoc
        mayM_ mint $ withPrefix space int
        space
        inter comma $ map prettyHSE ops

    prettyPrint (DefaultDecl _ types) = do
        write "default "
        listAutoWrap Other "(" ")" "," types

    prettyPrint (SpliceDecl _ expr) = pretty expr

    prettyPrint (TypeSig _ names ty) =
        onside $ prettyTypesig Declaration names ty

#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (PatSynSig _
                           names
                           mtyvarbinds
                           mcontext
                           mtyvarbinds'
                           mcontext'
                           ty) = depend "pattern" $ do
        listAutoWrap' Declaration "," names
        operator Declaration "::"
        mapM_ prettyForall mtyvarbinds
        mayM_ mcontext pretty
        mapM_ prettyForall mtyvarbinds'
        mayM_ mcontext' pretty
        pretty ty
#elif MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (PatSynSig _ names mtyvarbinds mcontext mcontext' ty) =
        depend "pattern" $ do
            listAutoWrap' Declaration "," names
            operator Declaration "::"
            mapM_ prettyForall mtyvarbinds
            mayM_ mcontext pretty
            mayM_ mcontext' pretty
            pretty ty
#else
    prettyPrint (PatSynSig _ name mtyvarbinds mcontext mcontext' ty) =
        depend "pattern" $ do
            pretty name
            operator Declaration "::"
            mapM_ prettyForall mtyvarbinds
            mayM_ mcontext pretty
            mayM_ mcontext' pretty
            pretty ty
#endif

    prettyPrint (FunBind _ matches) =
        withComputedTabStop stopRhs cfgAlignMatches measureMatch matches $
        linedOnside matches

    prettyPrint (PatBind _ pat rhs mbinds) = do
        onside $ do
            pretty pat
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds

    prettyPrint (PatSyn _ pat pat' patternsyndirection) = do
        depend "pattern" $ prettySimpleDecl pat sep pat'
        case patternsyndirection of
            ExplicitBidirectional _ decls ->
                prettyBinds (BDecls noNodeInfo decls)
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
            prettyTypesig Declaration [ name ] ty

    prettyPrint (ForExp _ callconv mstring name ty) =
        depend "foreign export" $ do
            pretty callconv
            mayM_ mstring $ withPrefix space (string . show)
            space
            prettyTypesig Declaration [ name ] ty

    prettyPrint (RulePragmaDecl _ rules) =
        if null rules
        then prettyPragma' "RULES" Nothing
        else prettyPragma "RULES" $ mapM_ pretty rules

    prettyPrint (DeprPragmaDecl _ deprecations) =
        if null deprecations
        then prettyPragma' "DEPRECATED" Nothing
        else prettyPragma "DEPRECATED" $ forM_ deprecations $
            \(names, str) -> do
                unless (null names) $ do
                    listAutoWrap' Other "," names
                    space
                string (show str)

    prettyPrint (WarnPragmaDecl _ warnings) =
        if null warnings
        then prettyPragma' "WARNING" Nothing
        else prettyPragma "WARNING" $ forM_ warnings $ \(names, str) -> do
            unless (null names) $ do
                listAutoWrap' Other "," names
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
            listAutoWrap' Declaration "," types

    prettyPrint (SpecInlineSig _ inline mactivation qname types) =
        prettyPragma name $ do
            mayM_ mactivation $ withPostfix space pretty
            pretty qname
            operator Declaration "::"
            listAutoWrap' Declaration "," types
      where
        name = if inline then "SPECIALISE INLINE" else "SPECIALISE NOINLINE"

    prettyPrint (InstSig _ instrule) =
        prettyPragma "SPECIALISE instance" $ pretty instrule

    prettyPrint (AnnPragma _ annotation) =
        prettyPragma "ANN" $ pretty annotation

    prettyPrint (MinimalPragma _ mbooleanformula) =
        prettyPragma "MINIMAL" $ mapM_ pretty mbooleanformula

    -- prettyPrint (RoleAnnotDecl _ qname roles) = undefined
    prettyPrint decl = prettyHSE decl

instance Pretty DeclHead where
    prettyPrint (DHead _ name) = pretty name

    prettyPrint (DHInfix _ tyvarbind name) = do
        pretty tyvarbind
        pretty $ VarOp noNodeInfo name

    prettyPrint (DHParen _ declhead) = parens $ pretty declhead

    prettyPrint (DHApp _ declhead tyvarbind) = depend' (pretty declhead) $
        pretty tyvarbind

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
    prettyPrint (BDecls _ decls) =
        withComputedTabStop stopRhs cfgAlignWhere measureDecl decls $
        prettyDecls skipBlankDecl DeclWhere decls

    prettyPrint (IPBinds _ ipbinds) = linedOnside ipbinds

instance Pretty IPBind where
    prettyPrint (IPBind _ ipname expr) = prettySimpleDecl ipname "=" expr

instance Pretty InjectivityInfo where
    prettyPrint (InjectivityInfo _ name names) = do
        operator Declaration "|"
        pretty name
        operator Declaration "->"
        inter space $ map pretty names

instance Pretty ResultSig where
    prettyPrint (KindSig _ kind) =
        withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "::"
            pretty kind

        vertical = do
            operatorV Declaration "::"
            pretty kind

    prettyPrint (TyVarSig _ tyvarbind) =
        withLayout cfgLayoutDeclaration flex vertical
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

    prettyPrint (ClsDefSig _ name ty) = do
        write "default"
        space
        prettyTypesig Declaration [ name ] ty

instance Pretty InstDecl where
    prettyPrint (InsDecl _ decl) = pretty decl

    prettyPrint (InsType _ ty ty') =
        depend "type" $ prettySimpleDecl ty "=" ty'

    prettyPrint (InsData _ dataornew ty qualcondecls derivings) =
        depend' (pretty dataornew) $ do
            pretty ty
            unless (null qualcondecls) $ prettyConDecls qualcondecls
            mapM_ pretty derivings

    prettyPrint (InsGData _ dataornew ty mkind gadtdecls derivings) = do
        depend' (pretty dataornew) $ do
            pretty ty
            mayM_ mkind $ \kind -> do
                operator Declaration "::"
                pretty kind
            write " where"
            newline
            lined gadtdecls
        mapM_ pretty derivings

instance Pretty Deriving where
#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (Deriving _ mderivstrategy instrules) =
        withIndentBy cfgIndentDeriving $ do
            write "deriving "
            prettyStratBefore
            case instrules of
                [ i@IRule{} ] -> pretty i
                [ IParen _ i ] -> listAutoWrap Other "(" ")" "," [ i ]
                _ -> listAutoWrap Other "(" ")" "," instrules
            prettyStratAfter
      where
        (prettyStratBefore, prettyStratAfter) = case mderivstrategy of
#if MIN_VERSION_haskell_src_exts(1,21,0)
            Just x@DerivVia{} -> (return (), space *> pretty x)
#endif
            Just x -> (pretty x <* space, return ())
            _ -> (return (), return ())
#else
    prettyPrint (Deriving _ instrules) = withIndentBy cfgIndentDeriving $ do
        write "deriving "
        case instrules of
            [ i@IRule{} ] -> pretty i
            [ IParen _ i ] -> listAutoWrap Other "(" ")" "," [ i ]
            _ -> listAutoWrap Other "(" ")" "," instrules
#endif

instance Pretty ConDecl where
    prettyPrint (ConDecl _ name types) = do
        pretty name
        unless (null types) $ do
            space
            oneline hor <|> ver
      where
        hor = inter space $ map pretty types

        ver = aligned $ linedOnside types

    prettyPrint (InfixConDecl _ ty name ty') = do
        pretty ty
        pretty $ ConOp noNodeInfo name
        pretty ty'

    prettyPrint (RecDecl _ name fielddecls) =
        prettyRecord len Declaration name fielddecls
      where
        len (FieldDecl _ names _) = measure $ inter comma $ map pretty names

instance Pretty FieldDecl where
    prettyPrint (FieldDecl _ names ty) = prettyTypesig Declaration names ty

instance Pretty QualConDecl where
    prettyPrint (QualConDecl _ mtyvarbinds mcontext condecl) = do
        mapM_ prettyForall mtyvarbinds
        mapM_ pretty mcontext
        pretty condecl

instance Pretty GadtDecl where
#if MIN_VERSION_haskell_src_exts(1,21,0)
    prettyPrint (GadtDecl _ name _ _ mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            prettyRecordFields len Declaration decls
            operator Type "->"
        pretty ty
#else
    prettyPrint (GadtDecl _ name mfielddecls ty) = do
        pretty name
        operator Declaration "::"
        mayM_ mfielddecls $ \decls -> do
            prettyRecordFields len Declaration decls
            operator Type "->"
        pretty ty
#endif
      where
        len (FieldDecl _ names _) = measure $ inter comma $ map pretty names

instance Pretty Match where
    prettyPrint (Match _ name pats rhs mbinds) = do
        onside $ do
            prettyApp name pats
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds

    prettyPrint (InfixMatch _ pat name pats rhs mbinds) = do
        onside $ do
            withLayout cfgLayoutInfixApp flex vertical
            atTabStop stopRhs
            pretty rhs
        mapM_ prettyBinds mbinds
      where
        flex = do
            pretty pat
            withOperatorFormatting Pattern
                                   (opName'' name)
                                   (prettyHSE $ VarOp noNodeInfo name)
                                   id
            inter spaceOrNewline $ map (cut . pretty) pats

        vertical = do
            pretty pat
            withOperatorFormattingV Pattern
                                    (opName'' name)
                                    (prettyHSE $ VarOp noNodeInfo name)
                                    id
            linedOnside pats

instance Pretty Rhs where
    prettyPrint (UnGuardedRhs _ expr) =
        cut $ withLayout cfgLayoutDeclaration flex vertical
      where
        flex = do
            operator Declaration "="
            pretty expr

        vertical = do
            operatorV Declaration "="
            pretty expr

    prettyPrint (GuardedRhss _ guardedrhss) =
        withIndent cfgIndentMultiIf $ linedOnside guardedrhss

instance Pretty GuardedRhs where
    prettyPrint (GuardedRhs _ stmts expr) = do
        prettyGuard stmts
        atTabStop stopRhs
        withLayout cfgLayoutDeclaration (operator d op) (operatorV d op)
        pretty expr
      where
        d = Declaration
        op = "="

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

#if MIN_VERSION_haskell_src_exts(1,22,0)
instance Pretty Asst where
    prettyPrint (TypeA _ ty) = pretty ty
    prettyPrint (IParam _ ipname ty) = prettyTypesig Declaration [ ipname ] ty
    prettyPrint (ParenA _ asst) = parens $ pretty asst

#else
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
        withOperatorFormatting Type
                               (opName' qname)
                               (prettyHSE $ QVarOp noNodeInfo qname)
                               id
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
#endif

instance Pretty Type where
    prettyPrint t = do
        layout <- gets psTypeLayout
        case layout of
            TypeFree -> withLayout cfgLayoutType flex vertical
            TypeFlex -> prettyF t
            TypeVertical -> prettyV t
      where
        flex = withTypeLayout TypeFlex $ prettyF t

        vertical = withTypeLayout TypeVertical $ prettyV t

        withTypeLayout :: TypeLayout -> Printer () -> Printer ()
        withTypeLayout l p = do
            layout <- gets psTypeLayout
            modify $ \s -> s { psTypeLayout = l }
            p
            modify $ \s -> s { psTypeLayout = layout }

        prettyF (TyForall _ mtyvarbinds mcontext ty) = do
            mapM_ prettyForall mtyvarbinds
            mapM_ pretty mcontext
            pretty ty

        prettyF (TyFun _ ty ty') = do
            pretty ty
            operator Type "->"
            pretty ty'

        prettyF (TyTuple _ boxed tys) = case boxed of
            Unboxed -> list Type "(#" "#)" "," tys
            Boxed -> list Type "(" ")" "," tys

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyF (TyUnboxedSum _ tys) = list Type "(#" "#)" "|" tys
#endif

        prettyF (TyList _ ty) = group Type "[" "]" $ pretty ty

        prettyF (TyParArray _ ty) = group Type "[:" ":]" $ pretty ty

        prettyF ty@TyApp{} = case flattenApp flatten ty of
            ctor : args -> prettyApp ctor args
            [] -> error "impossible"
          where
            flatten (TyApp _ a b) = Just (a, b)
            flatten _ = Nothing

        prettyF (TyVar _ name) = pretty name

        prettyF (TyCon _ qname) = pretty qname

        prettyF (TyParen _ ty) = parens . withTypeLayout TypeFree $ pretty ty

#if MIN_VERSION_haskell_src_exts(1,20,0)
        prettyF (TyInfix _ ty op ty') = do
            pretty ty
            withOperatorFormatting Type opname (prettyHSE op) id
            pretty ty'
          where
            opname = opName' $ case op of
                PromotedName _ qname -> qname
                UnpromotedName _ qname -> qname
#else
        prettyF (TyInfix _ ty qname ty') = do
            pretty ty
            withOperatorFormatting Type (opName' qname) (prettyHSE qname) id
            pretty ty'
#endif

        prettyF (TyKind _ ty kind) = do
            pretty ty
            operator Type "::"
            pretty kind

        prettyF (TyPromoted _ promoted) = pretty promoted

        prettyF (TyEquals _ ty ty') = do
            pretty ty
            operator Type "~"
            pretty ty'

        prettyF (TySplice _ splice) = pretty splice

        prettyF (TyBang _ bangtype unpackedness ty) = do
            pretty unpackedness
            pretty bangtype
            pretty ty

        prettyF ty@(TyWildCard _ _mname) = prettyHSE ty -- FIXME

        prettyF (TyQuasiQuote _ str str') = do
            write "["
            string str
            write "|"
            string str'
            write "|]"

#if MIN_VERSION_haskell_src_exts(1,21,0)
        prettyF (TyStar _) = write "*"
#endif

        prettyV (TyForall _ mtyvarbinds mcontext ty) = do
            forM_ mtyvarbinds $ \tyvarbinds -> do
                prettyForall tyvarbinds
            forM_ mcontext $ \context -> do
                case context of
                    (CxSingle _ asst) -> pretty asst
                    (CxTuple _ assts) -> list Type "(" ")" "," assts
                    (CxEmpty _) -> write "()"
                operatorV Type "=>"
            prettyV ty

        prettyV (TyFun _ ty ty') = do
            pretty ty
            operatorV Type "->"
            prettyV ty'

        prettyV ty = prettyF ty

#if !MIN_VERSION_haskell_src_exts(1,21,0)
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
#endif

instance Pretty Promoted where
    prettyPrint (PromotedInteger _ _ str) = string str

    prettyPrint (PromotedString _ _ str) = do
        write "\""
        string str
        write "\""

    prettyPrint (PromotedCon _ quote qname) = do
        when quote $ write "'"
        pretty qname

    prettyPrint (PromotedList _ quote tys) = do
        when quote $ write "'"
        list Expression "[" "]" "," tys

    prettyPrint (PromotedTuple _ tys) = do
        write "'"
        list Expression "(" ")" "," tys

    prettyPrint (PromotedUnit _) = write "'()"

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

flexibleOneline :: Printer a -> Printer a
flexibleOneline p = do
    allowOneline <- getOption cfgOptionFlexibleOneline
    if allowOneline then ignoreOneline p else p

instance Pretty Exp where
    prettyPrint (Var _ qname) = pretty qname

    prettyPrint (OverloadedLabel _ str) = do
        write "#"
        string str

    prettyPrint (IPVar _ ipname) = pretty ipname

    prettyPrint (Con _ qname) = pretty qname

    prettyPrint (Lit _ literal) = pretty literal

    prettyPrint e@(InfixApp _ _ qop _) =
        prettyInfixApp opName Expression $ flattenInfix flattenInfixApp e
      where
        flattenInfixApp (InfixApp _ lhs qop' rhs) =
            if compareAST qop qop' == EQ
            then Just (lhs, qop', rhs)
            else Nothing
        flattenInfixApp _ = Nothing

    prettyPrint e@App{} = case flattenApp flatten e of
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
        maybeSpace
        inter space $ map pretty pats
        flexibleOneline $ do
            operator Expression "->"
            pretty expr
      where
        maybeSpace = case pats of
            PIrrPat{} : _ -> space
            PBangPat{} : _ -> space
            _ -> return ()

    prettyPrint (Let _ binds expr) = withLayout cfgLayoutLet flex vertical
      where
        flex = do
            write "let "
            prettyOnside (CompactBinds binds)
            spaceOrNewline
            nl <- gets psNewline
            alignP <- getOption cfgOptionAlignLetBindsAndInExpr
            write $ if nl && alignP then "in  " else "in "
            prettyOnside expr

        vertical =
            withIndentAfter cfgIndentLet
                            (do
                                 write "let"
                                 withIndent cfgIndentLetBinds $
                                     pretty (CompactBinds binds))
                            (do
                                 newline
                                 alignP <- getOption cfgOptionAlignLetBindsAndInExpr
                                 write $ if alignP then "in " else "in"
                                 withIndent cfgIndentLetIn $ pretty expr)

    prettyPrint (If _ expr expr' expr'') = withLayout cfgLayoutIf flex vertical
      where
        flex = do
            write "if "
            prettyOnside expr
            spaceOrNewline
            write "then "
            prettyOnside expr'
            spaceOrNewline
            write "else "
            prettyOnside expr''

        vertical = withIndentAfter cfgIndentIf
                                   (do
                                        write "if "
                                        prettyOnside expr)
                                   (do
                                        newline
                                        write "then "
                                        prettyOnside expr'
                                        newline
                                        write "else "
                                        prettyOnside expr'')

    prettyPrint (MultiIf _ guardedrhss) = do
        write "if"
        withIndent cfgIndentMultiIf . linedOnside $ map GuardedAlt guardedrhss

    prettyPrint (Case _ expr alts) = do
        write "case "
        pretty expr
        write " of"
        if null alts
            then write " { }"
            else flexibleOneline . withIndent cfgIndentCase
                . withComputedTabStop stopRhs cfgAlignCase measureAlt alts $
                lined alts

    prettyPrint (Do _ stmts) = flexibleOneline $ do
        write "do"
        withIndent cfgIndentDo $ linedOnside stmts

    prettyPrint (MDo _ stmts) = flexibleOneline $ do
        write "mdo"
        withIndent cfgIndentDo $ linedOnside stmts

    prettyPrint (Tuple _ boxed exprs) = case boxed of
        Boxed -> list Expression "(" ")" "," exprs
        Unboxed -> list Expression "(#" "#)" "," exprs

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (UnboxedSum _ before after expr) = group Expression "(#" "#)"
        . inter space $ replicate before (write "|") ++ [ pretty expr ]
        ++ replicate after (write "|")
#endif

#if MIN_VERSION_haskell_src_exts(1,23,0)
    prettyPrint (ArrOp _ expr) = group Expression "(|" "|)" $ pretty expr
#endif

    prettyPrint (TupleSection _ boxed mexprs) = case boxed of
        Boxed -> list Expression "(" ")" "," $ map (MayAst noNodeInfo) mexprs
        Unboxed -> list Expression "(#" "#)" "," $
            map (MayAst noNodeInfo) mexprs

    prettyPrint (List _ exprs) = list Expression "[" "]" "," exprs

    prettyPrint (ParArray _ exprs) = list Expression "[:" ":]" "," exprs

    prettyPrint (Paren _ expr) = parens $ pretty expr

    prettyPrint (LeftSection _ expr qop) = parens $ do
        pretty expr
        operatorSectionL Expression (opName qop) $ prettyHSE qop

    prettyPrint (RightSection _ qop expr) = parens $ do
        operatorSectionR Expression (opName qop) $ prettyHSE qop
        pretty expr

    prettyPrint (RecConstr _ qname fieldupdates) =
        prettyRecord len Expression qname fieldupdates
      where
        len (FieldUpdate _ n _) = measure $ pretty n
        len (FieldPun _ n) = measure $ pretty n
        len (FieldWildcard _) = measure $ write ".."

    prettyPrint (RecUpdate _ expr fieldupdates) =
        prettyRecord len Expression expr fieldupdates
      where
        len (FieldUpdate _ n _) = measure $ pretty n
        len (FieldPun _ n) = measure $ pretty n
        len (FieldWildcard _) = measure $ write ".."

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

    prettyPrint (EnumFromThenTo _ expr expr' expr'') =
        group Expression "[" "]" $ do
            pretty expr
            comma
            pretty expr'
            operator Expression ".."
            pretty expr''

    prettyPrint (ParArrayFromTo _ expr expr') = group Expression "[:" ":]" $ do
        pretty expr
        operator Expression ".."
        pretty expr'

    prettyPrint (ParArrayFromThenTo _ expr expr' expr'') =
        group Expression "[:" ":]" $ do
            pretty expr
            comma
            pretty expr'
            operator Expression ".."
            pretty expr''

    prettyPrint (ListComp _ expr qualstmts) =
        withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[" "]" $ do
            prettyOnside expr
            operator Expression "|"
            list' Expression "," qualstmts

        vertical = groupV Expression "[" "]" $ do
            prettyOnside expr
            operatorV Expression "|"
            listV' Expression "," qualstmts

    prettyPrint (ParComp _ expr qualstmtss) =
        withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[" "]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operator Expression "|"
                list' Expression "," qualstmts

        vertical = groupV Expression "[" "]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operatorV Expression "|"
                listV' Expression "," qualstmts

    prettyPrint (ParArrayComp _ expr qualstmtss) =
        withLayout cfgLayoutListComp flex vertical
      where
        flex = group Expression "[:" ":]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operator Expression "|"
                list' Expression "," qualstmts

        vertical = groupV Expression "[:" ":]" $ do
            prettyOnside expr
            forM_ qualstmtss $ \qualstmts -> cut $ do
                operatorV Expression "|"
                listV' Expression "," qualstmts

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
        prettyPragma "GENERATED" $
            inter space
                  [ string $ show str
                  , int a
                  , write ":"
                  , int b
                  , write "-"
                  , int c
                  , write ":"
                  , int d
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

    prettyPrint (LCase _ alts) = flexibleOneline $ do
        write "\\case"
        if null alts
            then write " { }"
            else withIndent cfgIndentCase $
                withComputedTabStop stopRhs cfgAlignCase measureAlt alts $
                lined alts

#if !MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (ExprHole _) = write "_"
#endif

instance Pretty Alt where
    prettyPrint (Alt _ pat rhs mbinds) = do
        onside $ do
            pretty pat
            atTabStop stopRhs
            pretty $ GuardedAlts rhs
        mapM_ prettyBinds mbinds

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
        int $ fromIntegral integer

    prettyPrint p@(PInfixApp _ _ qname _) =
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

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (PUnboxedSum _ before after pat) = group Pattern "(#" "#)"
        . inter space $ replicate before (write "|") ++ [ pretty pat ]
        ++ replicate after (write "|")
#endif

    prettyPrint (PList _ pats) = list Pattern "[" "]" "," pats

    prettyPrint (PParen _ pat) = parens $ pretty pat

    prettyPrint (PRec _ qname patfields) = do
        withOperatorFormatting Pattern "record" (pretty qname) id
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

#if MIN_VERSION_haskell_src_exts(1,20,0)
    prettyPrint (PSplice _ splice) = pretty splice
#endif

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
        operator Expression "<-"
        pretty expr

    prettyPrint (Qualifier _ expr) = pretty expr

    prettyPrint (LetStmt _ binds) = do
        write "let "
        pretty $ CompactBinds binds

    prettyPrint (RecStmt _ stmts) = do
        write "rec "
        aligned $ linedOnside stmts

instance Pretty FieldUpdate where
    prettyPrint (FieldUpdate _ qname expr) = do
        pretty qname
        onside $ do
            atTabStop stopRecordField
            operator Expression "="
            pretty expr

    prettyPrint (FieldPun _ qname) = pretty qname

    prettyPrint (FieldWildcard _) = write ".."

instance Pretty QOp where
    prettyPrint qop =
        withOperatorFormatting Expression (opName qop) (prettyHSE qop) id

instance Pretty Op where
    prettyPrint (VarOp l name) = prettyPrint (QVarOp l (UnQual noNodeInfo name))
    prettyPrint (ConOp l name) = prettyPrint (QConOp l (UnQual noNodeInfo name))

instance Pretty Bracket where
    prettyPrint (ExpBracket _ expr) = group Expression "[|" "|]" $ pretty expr

#if MIN_VERSION_haskell_src_exts(1,22,0)
    prettyPrint (TExpBracket _ expr) =
        group Expression "[||" "||]" $ pretty expr
#endif

    prettyPrint (PatBracket _ pat) = group Expression "[p|" "|]" $ pretty pat

    prettyPrint (TypeBracket _ ty) = group Expression "[t|" "|]" $ pretty ty

    prettyPrint (DeclBracket _ decls) =
        group Expression "[d|" "|]" . aligned $ lined decls

instance Pretty Splice where
    prettyPrint (IdSplice _ str) = do
        write "$"
        string str

    prettyPrint (ParenSplice _ expr) = group Expression "$(" ")" $ pretty expr

#if MIN_VERSION_haskell_src_exts(1,22,0)
    prettyPrint (TIdSplice _ str) = do
        write "$$"
        string str

    prettyPrint (TParenSplice _ expr) = group Expression "$$(" ")" $ pretty expr
#endif

instance Pretty ModulePragma where
    prettyPrint (LanguagePragma _ names) =
        prettyPragma "LANGUAGE" $ listAutoWrap' Other "," names

    prettyPrint (OptionsPragma _ mtool str) = prettyPragma name $
        string (trim str)
      where
        name = case mtool of
            Just tool -> "OPTIONS_" `mappend` T.pack (HSE.prettyPrint tool)
            Nothing -> "OPTIONS"

        trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    prettyPrint (AnnModulePragma _ annotation) =
        prettyPragma "ANN" $ pretty annotation

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
    prettyPrint (ActiveFrom _ pass) = brackets $ int pass

    prettyPrint (ActiveUntil _ pass) = brackets $ do
        write "~"
        int pass

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
        listAutoWrap' Expression "," booleanformulas

    prettyPrint (OrFormula _ booleanformulas) =
        listAutoWrap' Expression "|" booleanformulas

    prettyPrint (ParenFormula _ booleanformula) = parens $ pretty booleanformula

-- Stick with HSE
#if MIN_VERSION_haskell_src_exts(1,20,0)
instance Pretty DerivStrategy
#endif

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
    deriving ( Functor, Annotated )

instance Pretty GuardedAlt where
    prettyPrint (GuardedAlt (GuardedRhs _ stmts expr)) = cut $ do
        operatorSectionR Pattern "|" $ write "|"
        listAutoWrap' Expression "," stmts
        operator Expression "->"
        pretty expr

newtype GuardedAlts l = GuardedAlts (Rhs l)
    deriving ( Functor, Annotated )

instance Pretty GuardedAlts where
    prettyPrint (GuardedAlts (UnGuardedRhs _ expr)) = cut $ do
        operator Expression "->"
        pretty expr

    prettyPrint (GuardedAlts (GuardedRhss _ guardedrhss)) =
        withIndent cfgIndentMultiIf $ linedOnside $ map GuardedAlt guardedrhss

newtype CompactBinds l = CompactBinds (Binds l)
    deriving ( Functor, Annotated )

instance Pretty CompactBinds where
    prettyPrint (CompactBinds (BDecls _ decls)) = aligned $
        withComputedTabStop stopRhs cfgAlignLetBinds measureDecl decls $
        lined decls
    prettyPrint (CompactBinds (IPBinds _ ipbinds)) =
        aligned $ linedOnside ipbinds

data MayAst a l = MayAst l (Maybe (a l))

instance Functor a => Functor (MayAst a) where
    fmap f (MayAst l x) = MayAst (f l) (fmap (fmap f) x)

instance Annotated a => Annotated (MayAst a) where
    ann (MayAst l x) = maybe l ann x

    amap f (MayAst l x) = MayAst (f l) (fmap (amap f) x)

instance (Annotated a, Pretty a) => Pretty (MayAst a) where
    prettyPrint (MayAst _ x) = mapM_ pretty x

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

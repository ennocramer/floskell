{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Floskell.Styles ( Style(..), styles ) where

import qualified Data.Map        as Map
import           Data.Text       ( Text )

import           Floskell.Config

-- | A printer style.
data Style =
    Style { styleName        :: !Text   -- ^ Name of the style, used in the commandline interface.
          , styleAuthor      :: !Text   -- ^ Author of the style definition.
          , styleDescription :: !Text   -- ^ Description of the style.
          , styleConfig      :: !Config -- ^ Style definition.
          }

chrisDoneCfg :: Config
chrisDoneCfg = safeConfig $
    defaultConfig { cfgIndent, cfgLayout, cfgOp, cfgGroup, cfgOptions }
  where
    cfgIndent =
        IndentConfig { cfgIndentOnside = 2
                     , cfgIndentDeriving = 2
                     , cfgIndentWhere = 2
                     , cfgIndentApp = Align
                     , cfgIndentCase = IndentBy 2
                     , cfgIndentClass = IndentBy 2
                     , cfgIndentDo = Align
                     , cfgIndentIf = IndentBy 3
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentWhereBinds = Align
                     , cfgIndentExportSpecList = IndentBy 2
                     , cfgIndentImportSpecList = IndentBy 7
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = TryOneline
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = TryOneline
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = Flex
                             , cfgLayoutRecord = Vertical
                             , cfgLayoutTypesig = TryOneline
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsNone WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing
          , Whitespace WsAfter WsAfter False
          )
        , ( ConfigMapKey (Just ".") (Just Type)
          , Whitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing, Whitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "<-") Nothing, Whitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just ":") Nothing, Whitespace WsNone WsBefore False)
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    Whitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides = []

    cfgOptions = OptionConfig { cfgOptionSortPragmas           = False
                              , cfgOptionSplitLanguagePragmas  = False
                              , cfgOptionSortImports           = False
                              , cfgOptionSortImportLists       = False
                              , cfgOptionPreserveVerticalSpace = False
                              }

cramerCfg :: Config
cramerCfg = safeConfig $
    defaultConfig { cfgAlign
                  , cfgIndent
                  , cfgLayout
                  , cfgOp
                  , cfgGroup
                  , cfgOptions
                  }
  where
    cfgAlign = AlignConfig { cfgAlignLimits       = (10, 25)
                           , cfgAlignCase         = False
                           , cfgAlignClass        = False
                           , cfgAlignImportModule = True
                           , cfgAlignImportSpec   = True
                           , cfgAlignLetBinds     = False
                           , cfgAlignRecordFields = True
                           , cfgAlignWhere        = False
                           }

    cfgIndent =
        IndentConfig { cfgIndentOnside = 4
                     , cfgIndentDeriving = 4
                     , cfgIndentWhere = 2
                     , cfgIndentApp = Align
                     , cfgIndentCase = IndentBy 4
                     , cfgIndentClass = IndentBy 4
                     , cfgIndentDo = IndentBy 4
                     , cfgIndentIf = Align
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = IndentBy 4
                     , cfgIndentMultiIf = IndentBy 4
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 4
                     , cfgIndentImportSpecList = AlignOrIndentBy 17
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = TryOneline
                             , cfgLayoutDeclaration = Flex
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutIf = TryOneline
                             , cfgLayoutImportSpecList = TryOneline
                             , cfgLayoutInfixApp = Flex
                             , cfgLayoutLet = TryOneline
                             , cfgLayoutListComp = TryOneline
                             , cfgLayoutRecord = TryOneline
                             , cfgLayoutTypesig = TryOneline
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing
          , Whitespace WsAfter WsNone False
          )
        , ( ConfigMapKey (Just ".") (Just Type)
          , Whitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing, Whitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "$") Nothing, Whitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just "@") Nothing, Whitespace WsNone WsNone False)
        , ( ConfigMapKey (Just "->") (Just Expression)
          , Whitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "record") (Just Pattern)
          , Whitespace WsNone WsNone False
          )
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    Whitespace WsBoth WsAfter False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey Nothing (Just Type), Whitespace WsNone WsAfter False)
        , ( ConfigMapKey Nothing (Just Pattern)
          , Whitespace WsNone WsAfter False
          )
        , (ConfigMapKey (Just "$(") Nothing, Whitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[|") Nothing, Whitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[d|") Nothing, Whitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[p|") Nothing, Whitespace WsNone WsNone False)
        , (ConfigMapKey (Just "[t|") Nothing, Whitespace WsNone WsNone False)
        , (ConfigMapKey (Just "(") Nothing, Whitespace WsNone WsAfter False)
        , ( ConfigMapKey (Just "(") (Just Other)
          , Whitespace WsBoth WsAfter False
          )
        , ( ConfigMapKey (Just "[") (Just Pattern)
          , Whitespace WsBoth WsAfter False
          )
        ]

    cfgOptions = OptionConfig { cfgOptionSortPragmas           = True
                              , cfgOptionSplitLanguagePragmas  = True
                              , cfgOptionSortImports           = True
                              , cfgOptionSortImportLists       = True
                              , cfgOptionPreserveVerticalSpace = True
                              }

gibianskyCfg :: Config
gibianskyCfg = safeConfig $
    defaultConfig { cfgAlign
                  , cfgIndent
                  , cfgLayout
                  , cfgOp
                  , cfgGroup
                  , cfgOptions
                  }
  where
    cfgAlign = AlignConfig { cfgAlignLimits       = (10, 25)
                           , cfgAlignCase         = True
                           , cfgAlignClass        = False
                           , cfgAlignImportModule = True
                           , cfgAlignImportSpec   = False
                           , cfgAlignLetBinds     = False
                           , cfgAlignRecordFields = False
                           , cfgAlignWhere        = False
                           }

    cfgIndent =
        IndentConfig { cfgIndentOnside = 2
                     , cfgIndentDeriving = 2
                     , cfgIndentWhere = 2
                     , cfgIndentApp = IndentBy 2
                     , cfgIndentCase = IndentBy 2
                     , cfgIndentClass = IndentBy 2
                     , cfgIndentDo = IndentBy 2
                     , cfgIndentIf = Align
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 4
                     , cfgIndentImportSpecList = IndentBy 4
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = Flex
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = Flex
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = TryOneline
                             , cfgLayoutRecord = TryOneline
                             , cfgLayoutTypesig = TryOneline
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing
          , Whitespace WsAfter WsAfter False
          )
        , ( ConfigMapKey (Just ".") (Just Type)
          , Whitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing, Whitespace WsBoth WsAfter False)
        , (ConfigMapKey (Just ":") Nothing, Whitespace WsNone WsBefore False)
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    Whitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey (Just "{") Nothing, Whitespace WsBoth WsAfter False) ]

    cfgOptions = OptionConfig { cfgOptionSortPragmas           = False
                              , cfgOptionSplitLanguagePragmas  = False
                              , cfgOptionSortImports           = False
                              , cfgOptionSortImportLists       = False
                              , cfgOptionPreserveVerticalSpace = False
                              }

johanTibellCfg :: Config
johanTibellCfg = safeConfig $
    defaultConfig { cfgIndent, cfgLayout, cfgOp, cfgGroup, cfgOptions }
  where
    cfgIndent =
        IndentConfig { cfgIndentOnside = 4
                     , cfgIndentDeriving = 4
                     , cfgIndentWhere = 2
                     , cfgIndentApp = IndentBy 4
                     , cfgIndentCase = IndentBy 4
                     , cfgIndentClass = IndentBy 4
                     , cfgIndentDo = IndentBy 4
                     , cfgIndentIf = IndentBy 4
                     , cfgIndentLet = Align
                     , cfgIndentLetBinds = Align
                     , cfgIndentLetIn = Align
                     , cfgIndentMultiIf = IndentBy 2
                     , cfgIndentWhereBinds = IndentBy 2
                     , cfgIndentExportSpecList = IndentBy 2
                     , cfgIndentImportSpecList = IndentBy 7
                     }

    cfgLayout = LayoutConfig { cfgLayoutApp = TryOneline
                             , cfgLayoutConDecls = Vertical
                             , cfgLayoutDeclaration = TryOneline
                             , cfgLayoutExportSpecList = TryOneline
                             , cfgLayoutIf = Vertical
                             , cfgLayoutImportSpecList = TryOneline
                             , cfgLayoutInfixApp = TryOneline
                             , cfgLayoutLet = Vertical
                             , cfgLayoutListComp = Flex
                             , cfgLayoutRecord = Vertical
                             , cfgLayoutTypesig = TryOneline
                             }

    cfgOp =
        OpConfig ConfigMap { cfgMapDefault   = Whitespace WsBoth WsBefore False
                           , cfgMapOverrides = Map.fromList opWsOverrides
                           }

    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsAfter WsBefore False)
        , ( ConfigMapKey (Just "record") Nothing
          , Whitespace WsAfter WsAfter True
          )
        , ( ConfigMapKey (Just ".") (Just Type)
          , Whitespace WsAfter WsAfter False
          )
        , (ConfigMapKey (Just "=") Nothing, Whitespace WsBoth WsAfter False)
        , ( ConfigMapKey (Just ":") (Just Pattern)
          , Whitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just ",") (Just Pattern)
          , Whitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just ",") (Just Other)
          , Whitespace WsNone WsBefore False
          )
        , ( ConfigMapKey (Just "record") (Just Pattern)
          , Whitespace WsAfter WsAfter False
          )
        ]

    cfgGroup =
        GroupConfig ConfigMap { cfgMapDefault   =
                                    Whitespace WsNone WsNone False
                              , cfgMapOverrides = Map.fromList groupWsOverrides
                              }

    groupWsOverrides =
        [ (ConfigMapKey (Just "{") Nothing, Whitespace WsBoth WsAfter False)
        , ( ConfigMapKey (Just "{") (Just Pattern)
          , Whitespace WsNone WsNone False
          )
        ]

    cfgOptions = OptionConfig { cfgOptionSortPragmas           = False
                              , cfgOptionSplitLanguagePragmas  = False
                              , cfgOptionSortImports           = False
                              , cfgOptionSortImportLists       = False
                              , cfgOptionPreserveVerticalSpace = False
                              }

-- | Base style definition.
base :: Style
base = Style { styleName        = "base"
             , styleAuthor      = "Enno Cramer"
             , styleDescription = "Configurable formatting style"
             , styleConfig      = safeConfig defaultConfig
             }

chrisDone :: Style
chrisDone = Style { styleName        = "chris-done"
                  , styleAuthor      = "Chris Done"
                  , styleDescription = "Chris Done's style"
                  , styleConfig      = chrisDoneCfg
                  }

cramer :: Style
cramer = Style { styleName        = "cramer"
               , styleAuthor      = "Enno Cramer"
               , styleDescription = "Enno Cramer's style"
               , styleConfig      = cramerCfg
               }

gibiansky :: Style
gibiansky = Style { styleName        = "gibiansky"
                  , styleAuthor      = "Andrew Gibiansky"
                  , styleDescription = "Andrew Gibiansky's style"
                  , styleConfig      = gibianskyCfg
                  }

johanTibell :: Style
johanTibell = Style { styleName        = "johan-tibell"
                    , styleAuthor      = "Johan Tibell"
                    , styleDescription = "Johan Tibell's style"
                    , styleConfig      = johanTibellCfg
                    }

-- | Styles list, useful for programmatically choosing.
styles :: [Style]
styles = [ base, chrisDone, johanTibell, gibiansky, cramer ]

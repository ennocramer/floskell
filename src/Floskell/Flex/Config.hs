{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Floskell.Flex.Config
    ( Indent(..)
    , LayoutContext(..)
    , WsLoc(..)
    , Whitespace(..)
    , Layout(..)
    , ConfigMap(..)
    , PenaltyConfig(..)
    , IndentConfig(..)
    , LayoutConfig(..)
    , OpConfig(..)
    , GroupConfig(..)
    , ModuleConfig(..)
    , FlexConfig(..)
    , defaultFlexConfig
    , cfgMapFind
    , cfgOpWs
    , cfgGroupWs
    , inWs
    , wsSpace
    , wsLinebreak
    ) where

import           Data.ByteString ( ByteString )
import           Data.Default    ( Default(..) )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import           Floskell.Types  ( ComInfoLocation(..) )

data Indent = Align
            | IndentBy !Int
            | AlignOrIndentBy !Int
    deriving (Eq, Ord, Show)

data LayoutContext = Declaration | Type | Pattern | Expression | Other
    deriving (Eq, Ord, Bounded, Enum, Show)

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving (Eq, Ord, Bounded, Enum, Show)

data Whitespace = Whitespace { wsSpaces         :: !WsLoc
                             , wsLinebreaks     :: !WsLoc
                             , wsForceLinebreak :: !Bool
                             }
    deriving (Show)

data Layout = Flex | Vertical | TryOneline
    deriving (Eq, Ord, Bounded, Enum, Show)

data ConfigMapKey = ConfigMapKey !(Maybe ByteString) !(Maybe LayoutContext)
    deriving (Eq, Ord, Show)

data ConfigMap a = ConfigMap { cfgMapDefault   :: !a
                             , cfgMapOverrides :: !(Map ConfigMapKey a)
                             }

data PenaltyConfig = PenaltyConfig { penaltyLinebreak    :: !Int
                                   , penaltyIndent       :: !Int
                                   , penaltyOverfull     :: !Int
                                   , penaltyOverfullOnce :: !Int
                                   }

instance Default PenaltyConfig where
    def = PenaltyConfig { penaltyLinebreak = 100
                        , penaltyIndent = 1
                        , penaltyOverfull = 10
                        , penaltyOverfullOnce = 200
                        }

data IndentConfig = IndentConfig { cfgIndentOnside         :: !Int
                                 , cfgIndentCase           :: !Indent
                                 , cfgIndentClass          :: !Indent
                                 , cfgIndentDo             :: !Indent
                                 , cfgIndentIf             :: !Indent
                                 , cfgIndentLet            :: !Indent
                                 , cfgIndentMultiIf        :: !Indent
                                 , cfgIndentWhere          :: !Indent
                                 , cfgIndentExportSpecList :: !Indent
                                 }

instance Default IndentConfig where
    def = IndentConfig { cfgIndentOnside = 4
                       , cfgIndentCase = IndentBy 4
                       , cfgIndentClass = IndentBy 4
                       , cfgIndentDo = IndentBy 4
                       , cfgIndentIf = IndentBy 0
                       , cfgIndentLet = Align
                       , cfgIndentMultiIf = IndentBy 4
                       , cfgIndentWhere = IndentBy 4
                       , cfgIndentExportSpecList = Align
                       }

data LayoutConfig = LayoutConfig { cfgLayoutExportSpecList :: !Layout
                                 , cfgLayoutImportSpecList :: !Layout
                                 , cfgLayoutDeriving       :: !Layout
                                 , cfgLayoutDeclaration    :: !Layout
                                 , cfgLayoutConDecl        :: !Layout
                                 , cfgLayoutRecDecl        :: !Layout
                                 , cfgLayoutTypesig        :: !Layout
                                 , cfgLayoutLet            :: !Layout
                                 , cfgLayoutIf             :: !Layout
                                 , cfgLayoutApp            :: !Layout
                                 , cfgLayoutInfixApp       :: !Layout
                                 }

instance Default LayoutConfig where
    def = LayoutConfig { cfgLayoutExportSpecList = Flex
                       , cfgLayoutImportSpecList = Flex
                       , cfgLayoutDeriving = Flex
                       , cfgLayoutDeclaration = Flex
                       , cfgLayoutConDecl = TryOneline
                       , cfgLayoutRecDecl = TryOneline
                       , cfgLayoutTypesig = Flex
                       , cfgLayoutLet = Flex
                       , cfgLayoutIf = Flex
                       , cfgLayoutApp = Flex
                       , cfgLayoutInfixApp = Flex
                       }

newtype OpConfig = OpConfig { unOpConfig :: ConfigMap Whitespace }

instance Default OpConfig where
    def = OpConfig ConfigMap { cfgMapDefault = Whitespace WsBoth WsBefore False
                             , cfgMapOverrides = Map.empty
                             }

newtype GroupConfig = GroupConfig { unGroupConfig :: ConfigMap Whitespace }

instance Default GroupConfig where
    def =
        GroupConfig ConfigMap { cfgMapDefault = Whitespace WsBoth WsAfter False
                              , cfgMapOverrides = Map.fromList overrides
                              }
      where
        overrides = [ ( ConfigMapKey (Just "(#") Nothing
                      , Whitespace WsBoth WsAfter False
                      )
                    ]

data ModuleConfig = ModuleConfig { cfgModuleSortPragmas          :: !Bool
                                 , cfgModuleSplitLanguagePragmas :: !Bool
                                 , cfgModuleSortImports          :: !Bool
                                 , cfgModuleAlignImports         :: !Bool
                                 , cfgModuleSortImportLists      :: !Bool
                                 }

instance Default ModuleConfig where
    def = ModuleConfig { cfgModuleSortPragmas = False
                       , cfgModuleSplitLanguagePragmas = False
                       , cfgModuleSortImports = False
                       , cfgModuleAlignImports = False
                       , cfgModuleSortImportLists = False
                       }

data FlexConfig = FlexConfig { cfgPenalty :: !PenaltyConfig
                             , cfgIndent  :: !IndentConfig
                             , cfgLayout  :: !LayoutConfig
                             , cfgOp      :: !OpConfig
                             , cfgGroup   :: !GroupConfig
                             , cfgModule  :: !ModuleConfig
                             }

instance Default FlexConfig where
    def = FlexConfig { cfgPenalty = def
                     , cfgIndent = def
                     , cfgLayout = def
                     , cfgOp = def
                     , cfgGroup = def
                     , cfgModule = def
                     }

defaultFlexConfig :: FlexConfig
defaultFlexConfig =
    def { cfgOp = OpConfig ((unOpConfig def) { cfgMapOverrides = Map.fromList opWsOverrides
                                             })
        }
  where
    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsAfter WsBefore False)
        ]

cfgMapFind :: LayoutContext -> ByteString -> ConfigMap a -> a
cfgMapFind ctx key ConfigMap{..} =
    let value = cfgMapDefault
        value' = Map.findWithDefault value
                                     (ConfigMapKey Nothing (Just ctx))
                                     cfgMapOverrides
        value'' = Map.findWithDefault value'
                                      (ConfigMapKey (Just key) Nothing)
                                      cfgMapOverrides
        value''' = Map.findWithDefault value''
                                       (ConfigMapKey (Just key) (Just ctx))
                                       cfgMapOverrides
    in
        value'''

cfgOpWs :: LayoutContext -> ByteString -> OpConfig -> Whitespace
cfgOpWs ctx op = cfgMapFind ctx op . unOpConfig

cfgGroupWs :: LayoutContext -> ByteString -> GroupConfig -> Whitespace
cfgGroupWs ctx op = cfgMapFind ctx op . unGroupConfig

inWs :: ComInfoLocation -> WsLoc -> Bool
inWs _ WsBoth = True
inWs Before WsBefore = True
inWs After WsAfter = True
inWs _ _ = False

wsSpace :: ComInfoLocation -> Whitespace -> Bool
wsSpace loc ws = loc `inWs` wsSpaces ws

wsLinebreak :: ComInfoLocation -> Whitespace -> Bool
wsLinebreak loc ws = loc `inWs` wsLinebreaks ws

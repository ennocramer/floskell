{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Floskell.Flex.Config where

import           Data.ByteString ( ByteString )
import           Data.Default    ( Default(..) )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import           Floskell.Types  ( ComInfoLocation(..) )

data Indent = Align
            | IndentBy !Int
            | AlignOrIndentBy !Int
    deriving (Eq, Show)

data LayoutContext = Declaration | Type | Pattern | Expression | Other
    deriving (Eq, Ord, Show)

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving (Eq, Show)

data Whitespace = Whitespace { wsSpaces         :: !WsLoc
                             , wsLinebreaks     :: !WsLoc
                             , wsForceLinebreak :: !Bool
                             }
    deriving (Show)

data ConfigMap a =
    ConfigMap { cfgMapDefault   :: !a
              , cfgMapOverrides :: !(Map (Maybe ByteString, Maybe LayoutContext) a)
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
                       , cfgIndentLet = Align
                       , cfgIndentMultiIf = IndentBy 4
                       , cfgIndentWhere = IndentBy 4
                       , cfgIndentExportSpecList = Align
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
        overrides = [ ((Just "(#", Nothing), Whitespace WsBoth WsAfter False) ]

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
                             , cfgOp      :: !OpConfig
                             , cfgGroup   :: !GroupConfig
                             , cfgModule  :: !ModuleConfig
                             }

instance Default FlexConfig where
    def = FlexConfig { cfgPenalty = def
                     , cfgIndent = def
                     , cfgOp = def
                     , cfgGroup = def
                     , cfgModule = def
                     }

defaultFlexConfig :: FlexConfig
defaultFlexConfig =
    def { cfgOp = OpConfig ((unOpConfig def) { cfgMapOverrides = Map.fromList opWsOverrides }) }
  where
    opWsOverrides = [ ((Just ",", Nothing), Whitespace WsAfter WsBefore False) ]

cfgMapFind :: LayoutContext -> ByteString -> ConfigMap a -> a
cfgMapFind ctx key ConfigMap{..} =
    let value = cfgMapDefault
        value' = Map.findWithDefault value (Nothing, Just ctx) cfgMapOverrides
        value'' = Map.findWithDefault value' (Just key, Nothing) cfgMapOverrides
        value''' = Map.findWithDefault value'' (Just key, Just ctx) cfgMapOverrides
    in value'''

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Floskell.Flex.Config where

import           Data.ByteString ( ByteString )
import           Data.Default    ( Default(..) )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import           Floskell.Types  ( ComInfoLocation(..) )

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving (Eq, Show)

data Whitespace = Whitespace { wsSpaces         :: !WsLoc
                             , wsLinebreaks     :: !WsLoc
                             , wsForceLinebreak :: !Bool
                             }
    deriving (Show)

data OpConfig = OpConfig { cfgOpWsDefault   :: !Whitespace
                         , cfgOpWsOverrides :: !(Map ByteString Whitespace)
                         }

instance Default OpConfig where
    def = OpConfig { cfgOpWsDefault = Whitespace WsBoth WsBefore False
                   , cfgOpWsOverrides = Map.fromList overrides
                   }
      where
        overrides = [ (",", Whitespace WsAfter WsBefore False) ]

data GroupConfig =
    GroupConfig { cfgGroupWsDefault   :: !Whitespace
                , cfgGroupWsOverrides :: !(Map ByteString Whitespace)
                }

instance Default GroupConfig where
    def = GroupConfig { cfgGroupWsDefault = Whitespace WsBoth WsAfter False
                      , cfgGroupWsOverrides = Map.fromList overrides
                      }
      where
        overrides = [ ("(#", Whitespace WsBoth WsAfter False) ]

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

data FlexConfig = FlexConfig { cfgOp     :: !OpConfig
                             , cfgGroup  :: !GroupConfig
                             , cfgModule :: !ModuleConfig
                             }

instance Default FlexConfig where
    def = FlexConfig { cfgOp = def
                     , cfgGroup = def
                     , cfgModule = def
                     }

defaultFlexConfig :: FlexConfig
defaultFlexConfig =
    def { cfgOp = def { cfgOpWsOverrides = Map.fromList opWsOverrides } }
  where
    opWsOverrides = [ (",", Whitespace WsAfter WsBefore False) ]

cfgOpWs :: ByteString -> OpConfig -> Whitespace
cfgOpWs op OpConfig{..} =
    Map.findWithDefault cfgOpWsDefault op cfgOpWsOverrides

cfgGroupWs :: ByteString -> GroupConfig -> Whitespace
cfgGroupWs op GroupConfig{..} =
    Map.findWithDefault cfgGroupWsDefault op cfgGroupWsOverrides

inWs :: ComInfoLocation -> WsLoc -> Bool
inWs _ WsBoth = True
inWs Before WsBefore = True
inWs After WsAfter = True
inWs _ _ = False

wsSpace :: ComInfoLocation -> Whitespace -> Bool
wsSpace loc ws = loc `inWs` wsSpaces ws

wsLinebreak :: ComInfoLocation -> Whitespace -> Bool
wsLinebreak loc ws = loc `inWs` wsLinebreaks ws

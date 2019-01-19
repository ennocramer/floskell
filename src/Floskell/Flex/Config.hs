{-# LANGUAGE DeriveGeneric     #-}
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
    , AlignConfig(..)
    , IndentConfig(..)
    , LayoutConfig(..)
    , OpConfig(..)
    , GroupConfig(..)
    , OptionConfig(..)
    , FlexConfig(..)
    , defaultFlexConfig
    , cfgMapFind
    , cfgOpWs
    , cfgGroupWs
    , inWs
    , wsSpace
    , wsLinebreak
    ) where

import           Data.Aeson         ( FromJSON(..), ToJSON(..)
                                    , genericParseJSON, genericToJSON )
import qualified Data.Aeson         as JSON
import           Data.Aeson.Types   as JSON ( Options(..), camelTo2
                                            , typeMismatch )
import           Data.ByteString    ( ByteString )
import           Data.Default       ( Default(..) )
import qualified Data.HashMap.Lazy  as HashMap
import           Data.Map.Strict    ( Map )
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T ( decodeUtf8, encodeUtf8 )

import           Floskell.Types     ( ComInfoLocation(..) )

import           GHC.Generics

data Indent = Align
            | IndentBy !Int
            | AlignOrIndentBy !Int
    deriving (Eq, Ord, Show, Generic)

data LayoutContext = Declaration | Type | Pattern | Expression | Other
    deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data WsLoc = WsNone | WsBefore | WsAfter | WsBoth
    deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data Whitespace = Whitespace { wsSpaces         :: !WsLoc
                             , wsLinebreaks     :: !WsLoc
                             , wsForceLinebreak :: !Bool
                             }
    deriving (Show, Generic)

data Layout = Flex | Vertical | TryOneline
    deriving (Eq, Ord, Bounded, Enum, Show, Generic)

data ConfigMapKey = ConfigMapKey !(Maybe ByteString) !(Maybe LayoutContext)
    deriving (Eq, Ord, Show)

data ConfigMap a = ConfigMap { cfgMapDefault   :: !a
                             , cfgMapOverrides :: !(Map ConfigMapKey a)
                             }
    deriving (Generic)

data PenaltyConfig = PenaltyConfig { penaltyLinebreak    :: !Int
                                   , penaltyIndent       :: !Int
                                   , penaltyOverfull     :: !Int
                                   , penaltyOverfullOnce :: !Int
                                   }
    deriving (Generic)

instance Default PenaltyConfig where
    def = PenaltyConfig { penaltyLinebreak = 100
                        , penaltyIndent = 1
                        , penaltyOverfull = 10
                        , penaltyOverfullOnce = 200
                        }

data AlignConfig = AlignConfig { cfgAlignLimits       :: !(Int, Int)
                               , cfgAlignCase         :: !Bool
                               , cfgAlignClass        :: !Bool
                               , cfgAlignImportModule :: !Bool
                               , cfgAlignImportSpec   :: !Bool
                               , cfgAlignLetBinds     :: !Bool
                               , cfgAlignRecordFields :: !Bool
                               , cfgAlignWhere        :: !Bool
                               }
    deriving (Generic)

instance Default AlignConfig where
    def = AlignConfig { cfgAlignLimits = (10, 25)
                      , cfgAlignCase = False
                      , cfgAlignClass = False
                      , cfgAlignImportModule = False
                      , cfgAlignImportSpec = False
                      , cfgAlignLetBinds = False
                      , cfgAlignRecordFields = False
                      , cfgAlignWhere = False
                      }

data IndentConfig = IndentConfig { cfgIndentOnside         :: !Int
                                 , cfgIndentDeriving       :: !Int
                                 , cfgIndentWhere          :: !Int
                                 , cfgIndentApp            :: !Indent
                                 , cfgIndentCase           :: !Indent
                                 , cfgIndentClass          :: !Indent
                                 , cfgIndentDo             :: !Indent
                                 , cfgIndentExportSpecList :: !Indent
                                 , cfgIndentIf             :: !Indent
                                 , cfgIndentImportSpecList :: !Indent
                                 , cfgIndentLet            :: !Indent
                                 , cfgIndentLetBinds       :: !Indent
                                 , cfgIndentLetIn          :: !Indent
                                 , cfgIndentMultiIf        :: !Indent
                                 , cfgIndentWhereBinds     :: !Indent
                                 }
    deriving (Generic)

instance Default IndentConfig where
    def = IndentConfig { cfgIndentOnside = 4
                       , cfgIndentDeriving = 4
                       , cfgIndentWhere = 2
                       , cfgIndentApp = Align
                       , cfgIndentCase = IndentBy 4
                       , cfgIndentClass = IndentBy 4
                       , cfgIndentDo = IndentBy 4
                       , cfgIndentExportSpecList = Align
                       , cfgIndentIf = Align
                       , cfgIndentImportSpecList = Align
                       , cfgIndentLet = Align
                       , cfgIndentLetBinds = Align
                       , cfgIndentLetIn = Align
                       , cfgIndentMultiIf = IndentBy 4
                       , cfgIndentWhereBinds = IndentBy 2
                       }

data LayoutConfig = LayoutConfig { cfgLayoutApp            :: !Layout
                                 , cfgLayoutConDecls       :: !Layout
                                 , cfgLayoutDeclaration    :: !Layout
                                 , cfgLayoutExportSpecList :: !Layout
                                 , cfgLayoutIf             :: !Layout
                                 , cfgLayoutImportSpecList :: !Layout
                                 , cfgLayoutInfixApp       :: !Layout
                                 , cfgLayoutLet            :: !Layout
                                 , cfgLayoutListComp       :: !Layout
                                 , cfgLayoutRecord         :: !Layout
                                 , cfgLayoutTypesig        :: !Layout
                                 }
    deriving (Generic)

instance Default LayoutConfig where
    def = LayoutConfig { cfgLayoutApp = Flex
                       , cfgLayoutConDecls = TryOneline
                       , cfgLayoutDeclaration = Flex
                       , cfgLayoutExportSpecList = Flex
                       , cfgLayoutIf = Flex
                       , cfgLayoutImportSpecList = Flex
                       , cfgLayoutInfixApp = Flex
                       , cfgLayoutLet = Flex
                       , cfgLayoutListComp = Flex
                       , cfgLayoutRecord = TryOneline
                       , cfgLayoutTypesig = Flex
                       }

newtype OpConfig = OpConfig { unOpConfig :: ConfigMap Whitespace }
    deriving (Generic)

instance Default OpConfig where
    def = OpConfig ConfigMap { cfgMapDefault = Whitespace WsBoth WsBefore False
                             , cfgMapOverrides = Map.empty
                             }

newtype GroupConfig = GroupConfig { unGroupConfig :: ConfigMap Whitespace }
    deriving (Generic)

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

data OptionConfig = OptionConfig { cfgOptionSortPragmas           :: !Bool
                                 , cfgOptionSplitLanguagePragmas  :: !Bool
                                 , cfgOptionSortImports           :: !Bool
                                 , cfgOptionSortImportLists       :: !Bool
                                 , cfgOptionPreserveVerticalSpace :: !Bool
                                 }
    deriving (Generic)

instance Default OptionConfig where
    def = OptionConfig { cfgOptionSortPragmas = False
                       , cfgOptionSplitLanguagePragmas = False
                       , cfgOptionSortImports = False
                       , cfgOptionSortImportLists = False
                       , cfgOptionPreserveVerticalSpace = False
                       }

data FlexConfig = FlexConfig { cfgPenalty :: !PenaltyConfig
                             , cfgAlign   :: !AlignConfig
                             , cfgIndent  :: !IndentConfig
                             , cfgLayout  :: !LayoutConfig
                             , cfgOp      :: !OpConfig
                             , cfgGroup   :: !GroupConfig
                             , cfgOptions :: !OptionConfig
                             }
    deriving (Generic)

instance Default FlexConfig where
    def = FlexConfig { cfgPenalty = def
                     , cfgAlign = def
                     , cfgIndent = def
                     , cfgLayout = def
                     , cfgOp = def
                     , cfgGroup = def
                     , cfgOptions = def
                     }

defaultFlexConfig :: FlexConfig
defaultFlexConfig =
    def { cfgOp = OpConfig ((unOpConfig def) { cfgMapOverrides = Map.fromList opWsOverrides
                                             })
        }
  where
    opWsOverrides =
        [ (ConfigMapKey (Just ",") Nothing, Whitespace WsAfter WsBefore False)
        , (ConfigMapKey (Just "record") Nothing, Whitespace WsAfter WsAfter False)
        , (ConfigMapKey (Just ".") (Just Type), Whitespace WsAfter WsAfter False)
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

------------------------------------------------------------------------
readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
    [ (x, "") ] -> Just x
    _ -> Nothing

enumOptions :: Int -> Options
enumOptions n =
    JSON.defaultOptions { constructorTagModifier = JSON.camelTo2 '-' . drop n }

recordOptions :: Int -> Options
recordOptions n =
    JSON.defaultOptions { fieldLabelModifier = JSON.camelTo2 '-' . drop n
                        , unwrapUnaryRecords = True
                        }

instance ToJSON Indent where
    toJSON i = JSON.String $ case i of
        Align -> "align"
        IndentBy x -> "indent-by " `T.append` T.pack (show x)
        AlignOrIndentBy x -> "align-or-indent-by " `T.append` T.pack (show x)

instance FromJSON Indent where
    parseJSON v@(JSON.String t) = maybe (JSON.typeMismatch "Indent" v) return $
        if t == "align"
        then Just Align
        else if "indent-by " `T.isPrefixOf` t
             then IndentBy <$> readMaybe (T.unpack $ T.drop 10 t)
             else if "align-or-indent-by " `T.isPrefixOf` t
                  then AlignOrIndentBy <$> readMaybe (T.unpack $ T.drop 19 t)
                  else Nothing

    parseJSON v = JSON.typeMismatch "Indent" v

instance ToJSON LayoutContext where
    toJSON = genericToJSON (enumOptions 0)

instance FromJSON LayoutContext where
    parseJSON = genericParseJSON (enumOptions 0)

instance ToJSON WsLoc where
    toJSON = genericToJSON (enumOptions 2)

instance FromJSON WsLoc where
    parseJSON = genericParseJSON (enumOptions 2)

instance ToJSON Whitespace where
    toJSON = genericToJSON (recordOptions 2)

instance FromJSON Whitespace where
    parseJSON = genericParseJSON (recordOptions 2)

instance ToJSON Layout where
    toJSON = genericToJSON (enumOptions 0)

instance FromJSON Layout where
    parseJSON = genericParseJSON (enumOptions 0)

layoutToText :: LayoutContext -> T.Text
layoutToText Declaration = "declaration"
layoutToText Type = "type"
layoutToText Pattern = "pattern"
layoutToText Expression = "expression"
layoutToText Other = "other"

textToLayout :: T.Text -> Maybe LayoutContext
textToLayout "declaration" = Just Declaration
textToLayout "type" = Just Type
textToLayout "pattern" = Just Pattern
textToLayout "expression" = Just Expression
textToLayout "other" = Just Other
textToLayout _ = Nothing

keyToText :: ConfigMapKey -> T.Text
keyToText (ConfigMapKey Nothing Nothing) = "default"
keyToText (ConfigMapKey (Just n) Nothing) = T.decodeUtf8 n
keyToText (ConfigMapKey Nothing (Just l)) = "* in " `T.append` layoutToText l
keyToText (ConfigMapKey (Just n) (Just l)) =
    T.decodeUtf8 n `T.append` " in " `T.append` layoutToText l

textToKey :: T.Text -> Maybe ConfigMapKey
textToKey t = case T.splitOn " in " t of
    [ "default" ] -> Just (ConfigMapKey Nothing Nothing)
    [ "*", "*" ] -> Just (ConfigMapKey Nothing Nothing)
    [ name ] -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing)
    [ name, "*" ] -> Just (ConfigMapKey (Just (T.encodeUtf8 name)) Nothing)
    [ "*", layout ] -> ConfigMapKey Nothing . Just <$> textToLayout layout
    [ name, layout ] -> ConfigMapKey (Just (T.encodeUtf8 name)) . Just <$> textToLayout layout
    _ -> Nothing

instance ToJSON a => ToJSON (ConfigMap a) where
    toJSON ConfigMap{..} = toJSON $ Map.insert "default" cfgMapDefault $
        Map.mapKeys keyToText cfgMapOverrides

instance FromJSON a => FromJSON (ConfigMap a) where
    parseJSON value = do
        o <- parseJSON value
        cfgMapDefault <- maybe (fail "Missing key: default") return $
                             HashMap.lookup "default" o
        cfgMapOverrides <- either fail (return . Map.fromList) $ sequence $
                               map toKey $
                                   HashMap.toList $ HashMap.delete "default" o
        return ConfigMap { .. }
      where
        toKey (k, v) = case textToKey k of
            Just k' -> Right (k', v)
            Nothing -> Left ("Invalid key: " ++ T.unpack k)

instance ToJSON PenaltyConfig where
    toJSON = genericToJSON (recordOptions 7)

instance FromJSON PenaltyConfig where
    parseJSON = genericParseJSON (recordOptions 7)

instance ToJSON AlignConfig where
    toJSON = genericToJSON (recordOptions 8)

instance FromJSON AlignConfig where
    parseJSON = genericParseJSON (recordOptions 8)

instance ToJSON IndentConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON IndentConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON LayoutConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON LayoutConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON OpConfig where
    toJSON = genericToJSON (recordOptions 0)

instance FromJSON OpConfig where
    parseJSON = genericParseJSON (recordOptions 0)

instance ToJSON GroupConfig where
    toJSON = genericToJSON (recordOptions 0)

instance FromJSON GroupConfig where
    parseJSON = genericParseJSON (recordOptions 0)

instance ToJSON OptionConfig where
    toJSON = genericToJSON (recordOptions 9)

instance FromJSON OptionConfig where
    parseJSON = genericParseJSON (recordOptions 9)

instance ToJSON FlexConfig where
    toJSON = genericToJSON (recordOptions 3)

instance FromJSON FlexConfig where
    parseJSON = genericParseJSON (recordOptions 3)

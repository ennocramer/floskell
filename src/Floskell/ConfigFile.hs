{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Floskell.ConfigFile
    ( AppConfig(..)
    , defaultAppConfig
    , findAppConfig
    , findAppConfigIn
    , readAppConfig
    , setStyle
    , setLanguage
    , setExtensions
    ) where

import           Control.Applicative   ( (<|>) )

import           Data.Aeson            ( (.:?), (.=), FromJSON(..), ToJSON(..) )
import qualified Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON ( typeMismatch )
import qualified Data.ByteString       as BS
import qualified Data.HashMap.Lazy     as HashMap
import           Data.List             ( inits )
import qualified Data.Text             as T

import           Floskell.Styles       ( styles )
import           Floskell.Types        ( Style(..) )

import           GHC.Generics          ( Generic )

import           Language.Haskell.Exts ( Extension(..), Language(..)
                                       , classifyExtension, classifyLanguage )

import           System.Directory
                 ( XdgDirectory(..), doesFileExist, findFileWith
                 , getAppUserDataDirectory, getCurrentDirectory
                 , getHomeDirectory, getXdgDirectory )
import           System.FilePath
                 ( joinPath, splitDirectories, takeDirectory )

data AppConfig = AppConfig { appStyle      :: Style
                           , appLanguage   :: Language
                           , appExtensions :: [Extension]
                           }
    deriving ( Generic )

instance ToJSON AppConfig where
    toJSON AppConfig{..} =
        JSON.object [ "style" .= styleName appStyle
                    , "language" .= show appLanguage
                    , "extensions" .= map showExt appExtensions
                    , "formatting" .= styleInitialState appStyle
                    ]
      where
        showExt (EnableExtension x) = show x
        showExt (DisableExtension x) = "No" ++ show x
        showExt (UnknownExtension x) = x

instance FromJSON AppConfig where
    parseJSON (JSON.Object o) = do
        style <- maybe (appStyle defaultAppConfig) lookupStyle <$> o .:? "style"
        language <- maybe (appLanguage defaultAppConfig) lookupLanguage
            <$> o .:? "language"
        extensions <- maybe (appExtensions defaultAppConfig)
                            (map lookupExtension) <$> o .:? "extensions"
        let fmt = styleInitialState style
        fmt' <- maybe fmt (updateConfig fmt) <$> o .:? "formatting"
        let style' = style { styleInitialState = fmt' }
        return $ AppConfig style' language extensions
      where
        updateConfig cfg v = case JSON.fromJSON $ mergeJSON (toJSON cfg) v of
            JSON.Error e -> error e
            JSON.Success x -> x

        mergeJSON JSON.Null r = r
        mergeJSON l JSON.Null = l
        mergeJSON (JSON.Object l) (JSON.Object r) =
            JSON.Object (HashMap.unionWith mergeJSON l r)
        mergeJSON _ r = r

    parseJSON v = JSON.typeMismatch "AppConfig" v

-- | Default program configuration.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig (head styles) Haskell2010 []

-- | Lookup a style by name.
lookupStyle :: String -> Style
lookupStyle name = case filter ((== T.pack name) . styleName) styles of
    [] -> error $ "Unknown style: " ++ name
    x : _ -> x

-- | Lookup a language by name.
lookupLanguage :: String -> Language
lookupLanguage name = case classifyLanguage name of
    UnknownLanguage _ -> error $ "Unknown language: " ++ name
    x -> x

-- | Lookup an extension by name.
lookupExtension :: String -> Extension
lookupExtension name = case classifyExtension name of
    UnknownExtension _ -> error $ "Unkown extension: " ++ name
    x -> x

-- | Try to find a configuration file based on current working
-- directory, or in one of the application configuration directories.
findAppConfig :: IO (Maybe FilePath)
findAppConfig = getCurrentDirectory >>= findAppConfigIn

findAppConfigIn :: FilePath -> IO (Maybe FilePath)
findAppConfigIn src = do
    isFile <- doesFileExist src
    let startFrom = if isFile then takeDirectory src else src

    dotfilePaths <- sequence [ getHomeDirectory, getXdgDirectory XdgConfig "" ]
    dotfileConfig <- findFileWith doesFileExist dotfilePaths ".floskell.json"
    userPaths <- sequence [ getAppUserDataDirectory "floskell"
                          , getXdgDirectory XdgConfig "floskell"
                          ]
    userConfig <- findFileWith doesFileExist userPaths "config.json"
    let localPaths =
            map joinPath . reverse . drop 1 . inits . splitDirectories $
            startFrom
    localConfig <- findFileWith doesFileExist localPaths "floskell.json"
    return $ localConfig <|> userConfig <|> dotfileConfig

-- | Load a configuration file.
readAppConfig :: FilePath -> IO AppConfig
readAppConfig file = do
    text <- BS.readFile file
    either (error . (++) (file ++ ": ")) return $ JSON.eitherDecodeStrict text

setStyle :: AppConfig -> Maybe String -> AppConfig
setStyle cfg mbStyle =
    cfg { appStyle = maybe (appStyle cfg) lookupStyle mbStyle }

setLanguage :: AppConfig -> Maybe String -> AppConfig
setLanguage cfg mbLanguage =
    cfg { appLanguage = maybe (appLanguage cfg) lookupLanguage mbLanguage }

setExtensions :: AppConfig -> [String] -> AppConfig
setExtensions cfg exts =
    cfg { appExtensions = appExtensions cfg ++ map lookupExtension exts }

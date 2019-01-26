{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Floskell.ConfigFile
    ( Config(..)
    , defaultConfig
    , findConfig
    , findConfigIn
    , readConfig
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

data Config = Config { cfgStyle      :: Style
                     , cfgLanguage   :: Language
                     , cfgExtensions :: [Extension]
                     }
    deriving ( Generic )

instance ToJSON Config where
    toJSON Config{..} = JSON.object [ "style" .= styleName cfgStyle
                                    , "language" .= show cfgLanguage
                                    , "extensions" .= map showExt cfgExtensions
                                    , "formatting" .= styleInitialState cfgStyle
                                    ]
      where
        showExt (EnableExtension x) = show x
        showExt (DisableExtension x) = "No" ++ show x
        showExt (UnknownExtension x) = x

instance FromJSON Config where
    parseJSON (JSON.Object o) = do
        style <- maybe (cfgStyle defaultConfig) lookupStyle <$> o .:? "style"
        language <- maybe (cfgLanguage defaultConfig) lookupLanguage
            <$> o .:? "language"
        extensions <- maybe (cfgExtensions defaultConfig) (map lookupExtension)
            <$> o .:? "extensions"
        let flex = styleInitialState style
        flex' <- maybe flex (updateFlexConfig flex) <$> o .:? "formatting"
        let style' = style { styleInitialState = flex' }
        return $ Config style' language extensions
      where
        updateFlexConfig cfg v =
            case JSON.fromJSON $ mergeJSON (toJSON cfg) v of
                JSON.Error e -> error e
                JSON.Success x -> x

        mergeJSON JSON.Null r = r
        mergeJSON l JSON.Null = l
        mergeJSON (JSON.Object l) (JSON.Object r) =
            JSON.Object (HashMap.unionWith mergeJSON l r)
        mergeJSON _ r = r

    parseJSON v = JSON.typeMismatch "Config" v

-- | Default program configuration.
defaultConfig :: Config
defaultConfig = Config (head styles) Haskell2010 []

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
findConfig :: IO (Maybe FilePath)
findConfig = getCurrentDirectory >>= findConfigIn

findConfigIn :: FilePath -> IO (Maybe FilePath)
findConfigIn src = do
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
readConfig :: FilePath -> IO Config
readConfig file = do
    text <- BS.readFile file
    either (error . (++) (file ++ ": ")) return $ JSON.eitherDecodeStrict text

setStyle :: Config -> Maybe String -> Config
setStyle cfg mbStyle =
    cfg { cfgStyle = maybe (cfgStyle cfg) lookupStyle mbStyle }

setLanguage :: Config -> Maybe String -> Config
setLanguage cfg mbLanguage =
    cfg { cfgLanguage = maybe (cfgLanguage cfg) lookupLanguage mbLanguage }

setExtensions :: Config -> [String] -> Config
setExtensions cfg exts =
    cfg { cfgExtensions = cfgExtensions cfg ++ map lookupExtension exts }

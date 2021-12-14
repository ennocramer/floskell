{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Floskell.ConfigFile
    ( AppConfig(..)
    , defaultAppConfig
    , findAppConfig
    , findAppConfigIn
    , readAppConfig
    , showStyle
    , showLanguage
    , showExtension
    , showFixity
    , lookupStyle
    , lookupLanguage
    , lookupExtension
    , lookupFixity
    , setStyle
    , setLanguage
    , setExtensions
    , setFixities
    ) where

import           Control.Applicative        ( (<|>) )

import           Data.Aeson
                 ( (.:?), (.=), FromJSON(..), ToJSON(..) )
import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.Types           as JSON ( typeMismatch )
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as JSON ( unionWith )
#endif
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import           Data.Char                  ( isLetter, isSpace )

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap          as Map
#else
import qualified Data.Aeson.Parser          as JSON ( json' )
import qualified Data.HashMap.Lazy          as Map
#endif

import           Data.List                  ( inits )
import qualified Data.Text                  as T

import           Floskell.Attoparsec        ( parseOnly )
import           Floskell.Styles            ( Style(..), styles )

import           GHC.Generics               ( Generic )

import           Language.Haskell.Exts
                 ( Extension(..), Fixity(..), Language(..), classifyExtension
                 , classifyLanguage )
import qualified Language.Haskell.Exts      as HSE

import           System.Directory
                 ( XdgDirectory(..), doesFileExist, findFileWith
                 , getAppUserDataDirectory, getCurrentDirectory
                 , getHomeDirectory, getXdgDirectory )
import           System.FilePath
                 ( joinPath, splitDirectories, takeDirectory )

data AppConfig = AppConfig { appStyle      :: Style
                           , appLanguage   :: Language
                           , appExtensions :: [Extension]
                           , appFixities   :: [Fixity]
                           }
    deriving ( Generic )

instance ToJSON AppConfig where
    toJSON AppConfig{..} =
        JSON.object [ "style" .= showStyle appStyle
                    , "language" .= showLanguage appLanguage
                    , "extensions" .= map showExtension appExtensions
                    , "fixities" .= map showFixity appFixities
                    , "formatting" .= styleConfig appStyle
                    ]

instance FromJSON AppConfig where
    parseJSON (JSON.Object o) = do
        style <- maybe (appStyle defaultAppConfig) lookupStyle <$> o .:? "style"
        language <- maybe (appLanguage defaultAppConfig) lookupLanguage
            <$> o .:? "language"
        extensions <- maybe (appExtensions defaultAppConfig)
                            (map lookupExtension) <$> o .:? "extensions"
        fixities <- maybe (appFixities defaultAppConfig) (map lookupFixity)
            <$> o .:? "fixities"
        let fmt = styleConfig style
        fmt' <- maybe fmt (updateConfig fmt) <$> o .:? "formatting"
        let style' = style { styleConfig = fmt' }
        return $ AppConfig style' language extensions fixities
      where
        updateConfig cfg v = case JSON.fromJSON $ mergeJSON (toJSON cfg) v of
            JSON.Error e -> error e
            JSON.Success x -> x

        mergeJSON JSON.Null r = r
        mergeJSON l JSON.Null = l
        mergeJSON (JSON.Object l) (JSON.Object r) =
            JSON.Object (Map.unionWith mergeJSON l r)
        mergeJSON _ r = r

    parseJSON v = JSON.typeMismatch "AppConfig" v

-- | Default program configuration.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig (head styles) Haskell2010 [] []

-- | Show name of a style.
showStyle :: Style -> String
showStyle = T.unpack . styleName

-- | Show a Haskell language name.
showLanguage :: Language -> String
showLanguage = show

-- | Show a Haskell language extension.
showExtension :: Extension -> String
showExtension (EnableExtension x) = show x
showExtension (DisableExtension x) = "No" ++ show x
showExtension (UnknownExtension x) = x

-- | Show a fixity declaration.
showFixity :: Fixity -> String
showFixity (Fixity assoc prec op) =
    showAssoc assoc ++ " " ++ show prec ++ " " ++ showOp op
  where
    showAssoc (HSE.AssocNone _) = "infix"
    showAssoc (HSE.AssocLeft _) = "infixl"
    showAssoc (HSE.AssocRight _) = "infixr"

    showOp (HSE.UnQual _ (HSE.Symbol _ symbol)) = symbol
    showOp (HSE.UnQual _ (HSE.Ident _ ident)) = "`" ++ ident ++ "`"
    showOp _ = error "Operator in fixity list not supported"

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

-- | Parse a fixity declaration.
lookupFixity :: String -> Fixity
lookupFixity decl =
    let (assoc, decl') = break isSpace $ dropWhile isSpace decl
        (prec, decl'') = break isSpace $ dropWhile isSpace decl'
        (op, _) = break isSpace $ dropWhile isSpace decl''
    in
        Fixity (readAssoc assoc) (read prec) (readOp op)
  where
    readAssoc "infix" = HSE.AssocNone ()
    readAssoc "infixl" = HSE.AssocLeft ()
    readAssoc "infixr" = HSE.AssocRight ()
    readAssoc assoc = error $ "Unknown associativity: " ++ assoc

    readOp op = HSE.UnQual () $ case op of
        '(' : op' -> HSE.Symbol () (init op')
        '`' : op' -> HSE.Ident () (init op')
        c : _ -> if isLetter c then HSE.Ident () op else HSE.Symbol () op
        _ -> error "Missing operator in infix declaration"

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
    either (error . (++) (file ++ ": ")) return $ eitherDecodeStrict text

setStyle :: AppConfig -> Maybe String -> AppConfig
setStyle cfg mbStyle =
    cfg { appStyle = maybe (appStyle cfg) lookupStyle mbStyle }

setLanguage :: AppConfig -> Maybe String -> AppConfig
setLanguage cfg mbLanguage =
    cfg { appLanguage = maybe (appLanguage cfg) lookupLanguage mbLanguage }

setExtensions :: AppConfig -> [String] -> AppConfig
setExtensions cfg exts =
    cfg { appExtensions = appExtensions cfg ++ map lookupExtension exts }

setFixities :: AppConfig -> [String] -> AppConfig
setFixities cfg fixities =
    cfg { appFixities = appFixities cfg ++ map lookupFixity fixities }

eitherDecodeStrict :: FromJSON a => BS.ByteString -> Either String a
eitherDecodeStrict i = case parseOnly jsonEOF' i of
    Right x -> case JSON.fromJSON x of
        JSON.Error e -> Left e
        JSON.Success x' -> Right x'
    Left e -> Left e
  where
    jsonEOF' = JSON.json' <* skipSpace <* AP.endOfInput

    skipSpace =
        AP.skipWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

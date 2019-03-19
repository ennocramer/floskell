{-# LANGUAGE TupleSections #-}

module Floskell.Imports ( sortImports, groupImports, splitImports ) where

import           Control.Monad.Trans.State    ( State, execState, gets, modify )

import           Data.Function                ( on )
import           Data.List
                 ( groupBy, inits, intercalate, sortOn, sortOn, unfoldr )
import qualified Data.Map                     as M
import           Data.Monoid                  ( First(..) )

import           Floskell.Config
                 ( ImportsGroup(..), ImportsGroupOrder(..) )

import           Language.Haskell.Exts.Syntax ( ImportDecl(..), ModuleName(..) )

moduleName :: ImportDecl a -> String
moduleName i = case importModule i of
    (ModuleName _ s) -> s

splitOn :: Char -> String -> [String]
splitOn c = unfoldr go
  where
    go [] = Nothing
    go x = Just $ drop 1 <$> break (== c) x

modulePrefixes :: String -> [String]
modulePrefixes = map (intercalate ".") . reverse . inits . splitOn '.'

data St a = St { stIndex  :: M.Map String Int
               , stGroups :: M.Map Int (ImportsGroup, [ImportDecl a])
               , stRest   :: [ImportDecl a]
               }

commonPrefixLength :: Eq a => [[a]] -> Int
commonPrefixLength = go 0
  where
    go l [] = l
    go l ([] : _) = l
    go l ((x : xs) : ys) =
        if all ((== [ x ]) . take 1) ys then go (l + 1) (xs : ys) else l

sortImports :: [ImportDecl a] -> [ImportDecl a]
sortImports = sortOn moduleName

groupImports :: Int -> [ImportDecl a] -> [[ImportDecl a]]
groupImports n = groupBy ((==) `on` prefix n)
  where
    prefix l = take 1 . drop l . splitOn '.' . moduleName

lookupFirst :: Ord a => [a] -> M.Map a b -> Maybe b
lookupFirst ks m = getFirst . mconcat $ map (First . (`M.lookup` m)) ks

placeImport :: ImportDecl a -> State (St a) ()
placeImport i = do
    idx <- gets (lookupFirst (modulePrefixes $ moduleName i) . stIndex)
    case idx of
        Just idx' -> modify $ \s -> s { stGroups = placeAt idx' (stGroups s) }
        Nothing -> modify $ \s -> s { stRest = stRest s ++ [ i ] }
  where
    placeAt = M.adjust (fmap (++ [ i ]))

splitImports :: [ImportsGroup] -> [ImportDecl a] -> [[ImportDecl a]]
splitImports groups imports = extract $
    execState (mapM_ placeImport imports) initial
  where
    initial = St { stIndex  = M.fromList . concat $
                       zipWith (\n g -> map (, n) (importsPrefixes g))
                               [ 0 .. ]
                               groups
                 , stGroups = M.fromList $
                       zipWith (\n g -> (n, (g, []))) [ 0 .. ] groups
                 , stRest   = []
                 }

    extract s = filter (not . null) $
        concatMap maybeSortAndGroup (M.elems $ stGroups s) ++ [ stRest s ]

    maybeSortAndGroup (g, is) = case importsOrder g of
        ImportsGroupKeep -> [ is ]
        ImportsGroupSorted -> [ sortImports is ]
        ImportsGroupGrouped -> groupImports (commonPrefixLength $
                                             importsPrefixes g) $ sortImports is

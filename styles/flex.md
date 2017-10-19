# Introduction

This file is a test suite. Each section maps to an HSpec test, and
each line that is followed by a Haskell code fence is tested to make
sure re-formatting that code snippet produces the same result.

You can browse through this document to see what Floskell's style is
like, or contribute additional sections to it, or regression tests.

# Modules

## Module Header

``` haskell
module Main where
```

## Export Lists

Without comments

``` haskell
module Main ( foo, bar, baz, main ) where
```

With comments

``` haskell
module Main ( 
              -- * Main Program
              main
              -- * Functions
            , foo -- foo function
            , bar -- bar function
            , baz ) where
```

## LANGUAGE Pragmas

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
```

``` haskell
{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}

module Main where
```

## Module Imports

``` haskell
import Prelude
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.ByteString ( ByteString, pack, unpack )
import qualified Data.ByteString as BS ( pack, unpack )
import Control.Monad hiding ( forM )
```

# Data Declarations

## Types

``` haskell
type EventSource a = ( AddHandler a, a -> IO () )
```

## Data and Newtypes

``` haskell
data Void

data Void = Void { absurd :: forall a. a }

data Empty = Empty

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Op a b = Op (b -> a)

data Enum = Foo | Bar | Baz

data Enum = Foo -- first
          | Bar -- second
          | Baz -- third
```

## Records

``` haskell
data Empty = Empty {}

data Op = Op { getOp :: b -> a }

data Point = Point { x :: Int, y :: Int, label :: String }
    deriving ( Eq, Show )

data Commented = Commented { singleField :: Int -- with a comment
                           }

data LongTypeSig = LongTypeSig { field :: ( IsString a, Monad m )
                                     => (ByteString -> ByteString)
                                     -> ByteString -> a -> m ()
                               }
```

# Type Classes

## Class Declarations

``` haskell
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Applicative m => Monad m where
    fail :: m a
    return :: a -> m a
    (>>=) :: a -> (a -> m b) -> m b

class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
    state :: (s -> ( a, s )) -> m a

class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: ( Generic a, GToJSON (Rep a) ) => a -> Value
    toJSON = genericToJSON defaultOptions
```

## Instance Declarations

``` haskell
instance ToJSON ()

instance Bounded Bool where
    minBound = False

    maxBound = True

instance Semigroup a =>Monoid (Maybe a) where
    mempty = Nothing

    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
```

## Type families

``` haskell
type family Id a

type family Id a = r | r -> a
```

``` haskell
type instance Id Int = Int
```

# Signatures

``` haskell
id :: a -> a
sort :: Ord a => [ a ] -> [ a ]
long :: ( IsString a, Monad m ) => ByteString
    -> ByteString -> ByteString -> ByteString -> ByteString -> a -> m ()
mkEncoderData
    :: DocumentType -> (Text -> Except String ByteString) -> EncoderData
codepageReference
    :: (ParserState -> Word8) -> AP.Parser Word8 -> Parser CodepageReference
mktime :: Int -- hours
     -> Int -- minutes
     -> Int -- seconds
     -> Time
transform :: forall a. St -> State St a -> EitherT ServantErr IO a
Implicit parameters

f :: (?x :: Int) => Int
```

# Expressions

## Tuples

``` haskell
empty :: ()
empty = ()

singleton :: (Int)
singleton = (1)

pair :: ( Int, String )
pair = ( 0, "Zero" )

lorem :: ( String, String )
lorem = ( "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        , "Curabitur nec ante nec mauris ornare suscipit."
        )

comment :: ( Int, Int, Int )
comment = ( 0 -- the first
          , 1 -- the second
          , 2
          )

match () = undefined
match (_) = undefined
match (x, y) = undefined
```

## Lists

``` haskell
empty :: [ a ]
empty = []

singleton :: [ String ]
singleton = [ "lorem" ]

short :: [ Int ]
short = [ 1, 2, 3, 4, 5 ]

lorem :: [ String ]
lorem = [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
        , "Curabitur nec ante nec mauris ornare suscipit."
        , "In ac vulputate libero."
        , "Duis eget magna non purus imperdiet molestie nec quis mauris."
        , "Praesent blandit quam vel arcu pellentesque, id aliquet turpis faucibus."
        ]

comment :: [ Int ]
comment = [ 1 -- the first
          , 2 -- the second
          , 3
          ]

match [] = undefined
match [_] = undefined
match [x, y] = undefined
```

## Records

``` haskell
origin = Point { x = 0, y = 0, label = "Origin" }

translate dx dy p = p { x = x p + dx, y = y p + dy }

config = config { configBasePath = defaultBasePath
                , configFileRegex = defaultFileRegex
                , configDelimiter = defaultDelimeter
                }

commented = config { configBasePath = "/" -- use root
                   }
```

## Let

``` haskell
foo = let x = x in x

foo = let x = 1
          y = 2 in x + y

foo = let expr = do
                  return () in expr

foo = let x = if True then False else True in x
```

## If-Then-Else

``` haskell
if True then False else True

if the condition evaluates
    to true then execute the first branch else execute the second branch

if cond -- comment
 then true else false

if cond then do
        return () else return ()

do
    if cond then true else false

do
    if cond -- comment
     then true else false
```

Multi-way if

``` haskell
x = if
         | x <- Just x, x <- Just x -> case x of
            Just x -> e
            Nothing -> p
         | otherwise -> e
```

## Case

``` haskell
strToMonth :: String -> Int
strToMonth month = case month of
        "Jan" -> 1
        "Feb" -> 2
        _ -> error $ "Unknown month " ++ month
```

## Do-Notation

``` haskell
main = do
        name <- getLine
        putStrLn $ "Hello " ++ name ++ "!"

main = repeatedly $ do
        getLine >>= putStrLn

main = repeatedly $ getline >>= \ s -> do
        putStrLn s

main = 
    -- comment
    do
        getLine >>= putStrLn
```

## Guards

``` haskell
fib x | x == 1 = 1
      | x == 2 = 1
      | otherwise = fib (x - 1) + fib (x - 2)

simple [] = True
simple [e] | simple e = True
simple _ = False
```

## List comprehensions

``` haskell
map f xs = [ f x | x <- xs ]

defaultExtensions = [ e | EnableExtension{extensionField1 = extensionField1}
                          <- knownExtensions knownExtensions
                        , let a = b
                          -- comment
                        , let c = d
                    ]

-- comment
defaultExtensions = [ e | e@EnableExtension{} <- knownExtensions
                    ] \\ map EnableExtension badExtensions
```

Parallel list comprehension

``` haskell
zip xs ys = [ ( x, y ) | x <- xs
                       | y <- ys
            ]

fun xs ys = [ ( alphaBetaGamma, deltaEpsilonZeta ) | x <- xs, z <- zs
                                                   | y <- ys, cond, let t = t
            ]
```

Transform list comprehensions

``` haskell
{-# LANGUAGE TransformListComp #-}

list = [ ( x, y, map the v ) | x <- [ 1 .. 10 ]
                             , y <- [ 1 .. 10 ]
                             , let v = x + y
                             , then group by v using groupWith
                             , then take 10
                             , then group using permutations
                             , t <- concat v
                             , then takeWhile by t < 3
       ]
```

## Operators

Applicative-style operators

``` haskell
x = Value <$> thing <*> secondThing <*> thirdThing
    <*> fourthThing <*> Just thisissolong <*> Just stilllonger <*> evenlonger
```

# Function declarations

## Where Clause

``` haskell
sayHello :: IO ()
sayHello = do
        name <- getLine
        putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"
```

## Guards and pattern guards

``` haskell
f :: Int
f x | x <- Just x, x <- Just x = case x of
           Just x -> e
    | otherwise = do
           e
  where
    x = y
```

## Case inside a `where` and `do`

``` haskell
g x = case x of
        a -> x
  where
    foo = case x of
            _ -> do
                    launchMissiles
      where
        y = 2
```

## Let inside a `where`

``` haskell
g x = let x = 1 in x
  where
    foo = let y = 2
              z = 3 in y
```
# Template Haskell

## Expression Brackets

``` haskell
add1 x = [| x + 1 |]
```

## Pattern Brackets

``` haskell
mkPat = [p| (x, y) |]
```

## Type Brackets

``` haskell
foo :: $( [t| Bool |] ) -> a
```

## Quasiquotes in types

``` haskell
fun :: [a|bc|]
```
# Comments

Haddock comments

``` haskell
-- | Module comment.
module X where

-- | Main doc.
main :: IO ()
main = return ()

data X = X -- ^ X is for xylophone.
       | Y -- ^ Y is for why did I eat that pizza.

data X = X { field1 :: Int -- ^ Field1 is the first field.
           , field11 :: Char
             -- ^ This field comment is on its own line.
           , field2 :: Int -- ^ Field2 is the second field.
           , field3 :: Char -- ^ This is a long comment which starts next to
             -- the field but continues onto the next line, it aligns exactly
             -- with the field name.
           , field4 :: Char-- ^ This is a long comment which starts on the following line
             -- from from the field, lines continue at the sme column.
           }
```

Comments around regular declarations

``` haskell
-- This is some random comment.
-- | Main entry point.
main = putStrLn "Hello, World!"-- This is another random comment.
```

Multi-line comments with multi-line contents

``` haskell
{- | This is some random comment.
Here is more docs and such.
Etc.
-}
main = putStrLn "Hello, World!"{- This is another random comment. -}
```

# Behaviour checks

## Symbols and Identifiers

``` haskell
{-# NOINLINE (<>) #-}
type API = api1 :<|> api2

type API = api1 S.:<|> api2

type API = (:<|>) api1 api2

type API = (S.:<|>) api1 api2

data T a = a :<|> a

data T a = (:<|>) a a

(++) a b = append a b

a ++ b = append a b

val = a ++ b

val = a V.++ b

val = (++) a b

val = (V.++) a b

val = a `or` b

val = a `L.or` b

f (a :+: b) = a
f (a C.:+: b) = a
f ((:+:) a b) = a
f ((C.:+:) a b) = a
f (a `C` b) = a
f (a `M.C` b) = a
```

``` haskell
data T = (-)

q = '(-)

data (-)

q = ''(-)
```

## Unboxed Tuples

``` haskell
{-# LANGUAGE UnboxedTuples #-}

f :: Int -> Int -> (# Int, Int #)
f x y = (# x + 1, y - 1 #)

g x = case f x x of
        (# a, b #) -> a + b

h x = let (# p, q #) = h x in undefined
```

## Lazy Patterns in a Lambda

``` haskell
f = \ ~a -> undefined -- \~a yields parse error on input ‘\~’
```

## Bang Patterns in a Lambda

``` haskell
f = \ !a -> undefined -- \!a yields parse error on input ‘\!’
```

## Binding Implicit Parameters

``` haskell
f = let ?x = 42 in f
```

## Unicode

``` haskell
α = γ * "ω" -- υ
```

## Empty Module

``` haskell

```

## Empty Case

``` haskell
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}

f1 = case () of { }

f2 = \case { }
```

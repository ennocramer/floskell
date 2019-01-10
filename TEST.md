# Introduction

This file acts both as a presentation of the Floskell formatting
styles, as well as a set of regression tests.

You can see how a particular style will format Haskell source by
reading the matching Markdown file in the styles/ directory.

For regression testing, the canonical source, TEST.md in the root
directory, is parsed and each Haskell code block formatted according
to all predefined styles.  The formatted output is then compared with
the corresponding, already formatted code block in the <style\>.md file
in the styles/ subdirectory.

The regression test will also verify that repeated invocations of the
pretty printer will not modify an already formatted piece of code.

The following code block acts as a quick presentation for the
different formatting styles, by presenting a mixture of common Haskell
constructs.

``` haskell
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Style.Haskell.Example

Haskell Code Style Example.
-}
module Style.Haskell.Example (
 -- * Types
 Enum(..)
 ,Either(..)
 ,Point(..)
 -- * Functions
 ,hello
 ) where

-- Module imports
import qualified Control.Monad.Trans.State (State,evalState,execState,get,modify,put,runState)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Prelude hiding (map)

-- Data declarations
data Enum=CaseA|CaseB|CaseC deriving(Eq,Enum,Show)

data Either a b=Left a|Right b deriving(Eq,Show)

data Point=Point{pointX::Float,pointY::Float,pointLabel::String}deriving(Eq,Show)

-- Type classes
class Functor f=>Applicative a where
    pure::b->a b
    ap::a (b->c)->a b->a c

class Fundep a b|a->b where
    convert::a->b

instance Functor f=>Functor(Wrap f)where
    fmap f (Wrap x)=Wrap $ fmap f x

-- Values
origin::Point
origin=Point{pointX=0,pointY=0,pointLabel="Origin"}

lorem::[String]
lorem=["Lorem ipsum dolor sit amet, consectetur adipiscing elit.",
 "Curabitur nec ante nec mauris ornare suscipit.",
 "In ac vulputate libero.",
 "Duis eget magna non purus imperdiet molestie nec quis mauris.",
 "Praesent blandit quam vel arcu pellentesque, id aliquet turpis faucibus."]

-- Functions
facs::[Int]
facs=[1,1]++zipWith(+)(tailfacs)

hello::MonadIO m=>m ()
hello=do name<-liftIO getLine
         liftIO . putStrLn $ greetings name
  where
    greetings n="Hello "++n++"!"

letExpr::Point->String
letExp x=let y=1
             z=2
  in if x>0 then y else z

ifExpr::Bool->Bool
ifExpr b=if b == True then False else True

caseExpr::[a]->Maybe a
caseExpr xs=case xs of
    [] -> Nothing
    (x:_) -> Just x

guarded::Int->Int
guarded x|x == 0=1
         |x == 1=1
         |otherwise=guarded (x - 2) + guarded (x - 1)

someLongFunctionNameWithALotOfParameters::
  (MonadIO m,MonadRandom m)=>String->(String->String)->m ()
someLongFunctionNameWithALotOfParameters=undefined
```

# Unit Tests

## ModuleHead and ExportSpecList

Without exports

``` haskell
module Main where
```

With exports

``` haskell
module Main (foo,bar,baz,main) where
```

With exports and comments

``` haskell
module Main (
  -- * Main Program
  main
  -- * Functions
  , foo -- foo function
  , bar -- bar function
  , baz -- baz function
  ) where
```

With deprecation

``` haskell
module Main {-# DEPRECATED "no longer supported" #-} where
```

With warnings

``` haskell
module Main {-# WARNING "do not use" #-} where
```

## ImportDecl

``` haskell
import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString (ByteString,pack,unpack)
import qualified Data.ByteString as BS (pack, unpack)
import Control.Monad hiding (forM)
```

## Decl

### TypeDecl

``` haskell
type Name = String
type Pair a = (a, a)
type Fun a b = a -> b
```

### DataDecl and GDataDecl

``` haskell
data Void
data Unit = Unit
data Maybe a = Nothing | Just a
data Num a => SomeNum = SomeNum a

newtype RWS r w s = RWS (ReaderT r (WriterT w (StateT s Identity)))
 deriving (Functor, Applicative, Monad)

data Enum =
    One   -- Foo
  | Two   -- Bar
  | Three -- Baz

data Foo deriving ()
data Foo deriving Show
data Foo deriving (Show)
data Foo deriving (Eq, Ord)

data Expr :: * -> * where
  Const :: Int -> Expr Int
  Plus :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool
  deriving (Show)

data Term a where
  Lit :: { val  :: Int } -> Term Int
  Succ :: { num  :: Term Int } -> Term Int
  Pred :: { num  :: Term Int } -> Term Int
  IsZero :: { arg  :: Term Int } -> Term Bool
  Pair :: { arg1 :: Term a, arg2 :: Term b } -> Term (a,b)
  If :: { cnd  :: Term Bool, tru  :: Term a, fls  :: Term a } -> Term a
```

### TypeFamDecl, TypeInsDecl, and ClosedTypeFamDecl

``` haskell
type family Mutable v
type family Mutable v = (r :: *)
type family Mutable v = r | r -> v

type instance Mutable Int = MIntVector

type family Store a where
  Store Bool = [Int]
  Store a = [a]

type family Store a = (r :: *) where
  Store a = [a]

type family Store a = r | r -> a where
  Store a = [a]
```

### DataFamDecl, DataInsDecl, and GDataInsDecl

``` haskell
data family List a

data instance List () = NilList Int
data instance List Char = CharNil | CharCons Char (List Char)
  deriving (Eq, Ord, Show)

data instance List Int :: * where
  IntNil :: List Int
  IntCons :: Int -> List Int
  deriving (Eq, Ord, Show)

data instance List Int :: * where
  IntNil :: List Int
  IntCons :: { val :: Int } -> List Int
  deriving (Eq, Ord, Show)
```

### ClassDecl and InstDecl

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
  state :: (s -> (a, s)) -> m a

class ToJSON a where
  toJSON :: a -> Value
  default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
  toJSON = genericToJSON defaultOptions

instance ToJSON ()

instance Bounded Bool where
  minBound = False
  maxBound = True

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

instance Data () where
  type Base = ()
  newtype Wrapped = Wrapped { unWrap :: () }
  data Expr :: * -> * where
    Const :: Int -> Expr Int
    Plus :: Expr Int -> Expr Int -> Expr Int
    Eq :: Expr Int -> Expr Int -> Expr Bool
```

### DerivDecl

``` haskell
deriving instance Eq a => Eq (Sum a)
deriving instance {-# OVERLAP #-} Eq a => Eq (Sum a)
deriving stock instance {-# OVERLAPS #-} Eq a => Eq (Sum a)
deriving anyclass instance {-# OVERLAPPING #-} Eq a => Eq (Sum a)
deriving newtype instance {-# OVERLAPPABLE #-} Eq a => Eq (Sum a)
```

### InfixDecl

``` haskell
infix 4 ==, /=, <, <=, >, >=
infixr 0 $
infixl !!
```

### DefaultDecl

``` haskell
default ()
default (Integer, Double)
```

### SpliceDecl

``` haskell
$foo
$(bar baz)
```

### TypeSig

``` haskell
id :: a -> a
sort :: Ord a => [a] -> [a]
long :: (IsString a, Monad m) => ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> a -> m ()
mktime :: Int -- hours
       -> Int -- minutes
       -> Int -- seconds
       -> Time
transform :: forall a. St -> State St a -> EitherT ServantErr IO a
```

### PatSyn and PatSynSig

``` haskell
{-# LANGUAGE PatternSynonyms #-}

pattern MyJust :: a -> Maybe a
pattern MyJust a = Just a

pattern MyPoint :: Int -> Int -> (Int, Int)
pattern MyPoint{x, y} = (x,y)

pattern ErrorCall :: String -> ErrorCall
pattern ErrorCall s <- ErrorCallWithLocation s _
  where
    ErrorCall s = ErrorCallWithLocation s ""

pattern IsTrue :: Show a => a
pattern IsTrue <- ((== "True") . show -> True)

pattern ExNumPat :: () => Show b => b -> T
pattern ExNumPat x = MkT x

pattern Foo, Bar :: Show a => a
```

### FunBind and PatBind

``` haskell
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RecordWildCards #-}

pi = 3.14

id x = x

not False = True
not _ = False

head (x : _) = x

maybe x _ Nothing = x
maybe _ f (Some x) = f x

fst (x, _) = x
fst' (# x, _ #) = x

fstPrism (# x | | #) = Just x
fstPrism (# | _ | #) = Nothing
fstPrism (# | | _ #) = Nothing

empty [] = True
empty _ = False

unSum (Sum { getSum = s }) = s

mag2 Point{x, y} = sqr x + sqr y
mag2 Point{..} = sqr x + sqr y

strict !x = x
irrefutable ~x = x

(//) a b = undefined
a // b = undefined

main = do
    greet "World"
  where
    greet who = putStrLn $ "Hello, " ++ who ++ "!"
```

### ForImp and ForExp

``` haskell
{-# LANGUAGE ForeignFunctionInterface #-}
foreign import ccall sin :: Double -> Double
foreign import ccall "sin" sin :: Double -> Double
foreign import ccall "sin" sin :: Double -> Double
foreign import ccall unsafe exit :: Double -> Double

foreign export ccall callback :: Int -> Int
```

### Pragmas

``` haskell
{-# RULES #-}
{-# RULES "map/map" forall f g xs. map f (map g xs) = map (f.g) xs #-}
{-# RULES "map/append" [2] forall f xs ys. map f (xs ++ ys) = map f xs ++ map f ys #-}

{-# DEPRECATED #-}
{-# DEPRECATED foo "use bar instead" #-}
{-# DEPRECATED foo, bar, baz "no longer supported" #-}

{-# WARNING #-}
{-# WARNING foo "use bar instead" #-}
{-# WARNING foo, bar, baz "no longer supported" #-}

{-# INLINE foo #-}
{-# INLINE [3] foo #-}
{-# INLINE [~3] foo #-}
{-# NOINLINE foo #-}

{-# INLINE CONLIKE foo #-}
{-# INLINE CONLIKE [3] foo #-}

{-# SPECIALISE foo :: Int -> Int #-}
{-# SPECIALISE [3] foo :: Int -> Int, Float -> Float #-}

{-# SPECIALISE INLINE foo :: Int -> Int #-}
{-# SPECIALISE NOINLINE foo :: Int -> Int #-}

{-# SPECIALISE instance Foo Int #-}
{-# SPECIALISE instance forall a. (Ord a) => Foo a #-}

{-# ANN foo (Just "Foo") #-}
{-# ANN type Foo (Just "Foo") #-}
{-# ANN module (Just "Foo") #-}

{-# MINIMAL foo | bar, (baz | quux) #-}
```

## Exp

### Var, Con, Lit, Tuple, UnboxedSum, List, and ExpTypeSig

``` haskell
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

foo = foo

foo = Nothing

foo = 123

foo = 'x'

foo = ""
foo = "Lorem Ipsum Dolor Amet Sit"

foo = ()
foo = (1, 2)
foo = (1 -- the one
 , 2)

foo = (# #)
foo = (# 1, 2 #)
foo = (# 1 -- the one
 , 2 #)


foo = (# 1 #)
foo = (# | 1 | | #)
foo = (# | 1 -- the one
      | | #)

foo = []
foo = [1]
foo = [1,2]
foo = [1 -- the one
 , 2]

foo = 1 :: Int
```

### App, InfixApp, NegApp, LeftSection, RightSection

``` haskell
foo = foldl fn init list
foo = foldl fn -- reducer
  init -- initial value
  list

foo = 1 + 2
foo = fn `map` list

foo = -3

foo = (+ arg)
foo = (`op` arg)

foo = (arg +)
foo = (arg `op`)
```

### EnumFrom, EnumFromTo, EnumFromThen, EnumFromThenTo, ParArrayFromTo, ParArrayFromThenTo

``` haskell
foo = [1..]
foo = [1..10]
foo = [1, 2..]
foo = [1, 2..10]
foo = [:1..10:]
foo = [:1, 2..10:]
```

### ListComp, ParComp, and ParArrayComp

``` haskell
{-# LANGUAGE TransformListComp #-}

foo = [ (x, y) | x <- xs, y <- ys ]
foo = [ (x, y) -- cartesian product
      | x <- xs -- first list
      , y <- ys -- second list
      ]
foo = [ (x,y) | x <- xs | y <- ys ]
foo = [ (x,y) -- zip
      | x <- xs -- first list
      | y <- ys -- second list
      ]
foo = [: (x,y) | x <- xs | y <- ys :]
foo = [: (x,y) -- zip
      | x <- xs -- first list
      | y <- ys -- second list
      :]

foo = [ (x, y)
      | x <- xs
      , y <- ys
      , then reverse
      , then sortWith by (x+y)
      , then group using permutations
      , then group by (x+y) using groupWith
      ]
```

### RecConstr and RecUpdate

``` haskell
{-# LANGUAGE RecordWildCards #-}

foo = Point { x = 1, y = 2 }
foo = Point { x = 1 -- the one
            , y
            , ..
            }

foo = bar { x = 1 }
foo = bar { x = 1 -- the one
          , y
          , ..
          }
```

### Let, If, MultiIf, and Case

``` haskell
{-# LANGUAGE MultiWayIf #-}

foo = let x = x in x

foo = let x = x -- bottom
 in
  -- bottom
  x

foo = if null xs then None else Some $ head xs

foo = if null xs -- condition
  then None -- it's empty
  else Some $ head xs -- it's not

foo = if | null xs -> None
         | otherwise -> Some $ head xs

foo = if | null xs ->
           -- it's empty
           None
         | otherwise ->
           -- it's not
           Some $ head x

foo = case x of
  True -> False
  False -> True

foo = case xs of
  [] ->
    -- it's empty
    None
  x : _ ->
    -- it's not
    Some x

foo = case xs of
    _ | null xs -> None
    _ -> Some $ head x
```

### Do and MDo

``` haskell
{-# LANGUAGE RecursiveDo #-}

foo = do { return () }

foo = do
  return ()

foo = do
  this <- that
  let this' = tail this
  if this -- condition
  then that
  else those

foo = mdo
  return ()
```

### Lambda, LCase

``` haskell
{-# LANGUAGE LambdaCase #-}

foo = \x -> x
foo = \ ~x -> x
foo = \ !x -> x
foo d = \case
  Nothing -> d
  Some x -> x
```

### BracketExp, SpliceExp, QuasiQuote, VarQuote, and TypQuote

``` haskell
{-# LANGUAGE TemplateHaskell #-}

mkDecl :: Q Decl
mkDecl = [d|id x = x|]

mkType :: Q Type
mkType = [t|(a, b) -> a|]

mkPat :: Q Pat
mkPat = [p|(a, b)|]

mkExp :: Q Exp
mkExp = [e|a|]

fst :: $(mkType)
fst $(mkPat) = $(mkExp)

html = [html|<p>Lorem Ipsum Dolor Amet Sit</p>|]

foo = mkSomething 'id 'Nothing ''Maybe
```

# Regression Tests

## Do

Before comments and onside indent do not mix well.

``` haskell
foo = do
  -- comment
  some expression
```

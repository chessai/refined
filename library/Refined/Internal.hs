{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-| This module is internal.
 - It exports the newtype constructor for 'Refined',
 - which allows the end user to potentially write
 - unsafe coercions with 'coerce'.
-}

module Refined.Internal
  ( Refined(..)
  , refine
  , refineTH
  , unrefine

  , Predicate(..)
  , Weaken(..)

  , Size
  , NonEmpty
  , Ascending
  , Descending

  , Not
  , And
  , Or

  , LessThan
  , GreaterThan
  , From
  , To
  , FromTo 
  , EqualTo
  , Positive
  , NonPositive
  , Negative
  , NonNegative
  , ZeroToOne
  ) where

import Control.Applicative
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Either (either)
import Data.Function (fix)
import Data.Map.Internal (Map(..))
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Typeable (Typeable)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits
import Text.Show (showString)

import qualified Data.List as List
import qualified Language.Haskell.TH.Syntax as TH

import Refined.Orphan ()

-- |
-- A refinement type, 
-- which wraps a value of type @x@,
-- ensuring that it satisfies a type-level predicate @p@.
newtype Refined p x = Refined x
  deriving
    ( Data
    , Eq
    , Foldable
    , Functor
    , Generic
    , Generic1
    , Ord
    , Show
    , Traversable
    , Typeable
    )

type role Refined phantom representational

instance Semigroup a => Semigroup (Refined p a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Refined p a) where
  mempty = Refined mempty
  mappend = liftA2 mappend

instance Applicative (Refined p) where
  pure = Refined
  Refined f <*> Refined a = Refined (f a)

instance Monad (Refined p) where
  return = Refined
  (Refined x) >>= inj = inj x

instance (Read x, Predicate p x) => Read (Refined p x) where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Refined", r2) <- lex r1
      (raw,       r3) <- readsPrec 11 r2
      case refine raw of
        Right val -> [(val, r3)]
        Left  _   -> []

instance TH.Lift x => TH.Lift (Refined p x) where
  lift (Refined a) =
    [|Refined a|]

-- |
-- A smart constructor of a 'Refined' value.
-- Checks the input value at runtime.
{-# INLINABLE refine #-}
refine :: Predicate p x => x -> Either String (Refined p x)
refine x =
  fix $ \result ->
    maybe (Right (Refined x)) Left $
    validate (predicateByResult result) x
  where
    -- A work-around for the type-inference.
    predicateByResult :: Either String (Refined p x) -> p
    predicateByResult =
      const undefined

-- |
-- Constructs a 'Refined' value with checking at compile-time using Template Haskell.
-- E.g.,
-- 
-- >>> $$(refineTH 23) :: Refined Positive Int
-- Refined 23
-- 
-- Here's an example of an invalid value:
-- 
-- >>> $$(refineTH 0) :: Refined Positive Int
-- <interactive>:6:4:
--     Value is not greater than 0
--     In the Template Haskell splice $$(refineTH 0)
--     In the expression: $$(refineTH 0) :: Refined Positive Int
--     In an equation for ‘it’:
--         it = $$(refineTH 0) :: Refined Positive Int
-- 
-- If it's not evident, the example above indicates a compile-time failure, 
-- which means that the checking was done at compile-time, 
-- thus introducing a zero runtime overhead compared to a plain value construction.
refineTH :: (Predicate p x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined p x))
refineTH =
  fix $ \loop ->
    fmap TH.TExp . either fail TH.lift . refineByResult (loop undefined)
  where
    -- A work-around for the type-inference.
    refineByResult :: Predicate p x => TH.Q (TH.TExp (Refined p x)) -> x -> Either String (Refined p x)
    refineByResult =
      const refine

-- |
-- Extracts the refined value.
{-# INLINE unrefine #-}
unrefine :: Refined p x -> x
unrefine = coerce

-- * Predicate
-------------------------

-- |
-- A class which defines a runtime interpretation of
-- a type-level predicate @p@ for type @x@.
class Predicate p x where
  -- |
  -- Check the value @x@ according to the predicate @p@,
  -- producing an error string if the value does not satisfy.
  validate :: p -> x -> Maybe String


-- * Rules
-------------------------

-- |
-- A predicate that checks whether or not something with an
-- 'IsList' instance is in ascending order.
data Ascending

instance (IsList t, Ord (Item t)) => Predicate Ascending t where
  validate _ x =
    if (List.sort (toList x) == (toList x))
      then Nothing
      else Just ("List is not in ascending order")

-- |
-- A predicate that checks whether or not something with an
-- 'IsList' instance is in descending order.
data Descending

instance (IsList t, Ord (Item t)) => Predicate Descending t where
  validate _ x =
    if (List.reverse (List.sort (toList x)) == toList x)
      then Nothing
      else Just ("List is not in descending order")

-- |
-- A predicate that checks whether or not the length of something
-- with an 'IsList' instance is equal to n.
data LengthList (n :: Nat)

type EmptyList = LengthList 0

instance (IsList t, KnownNat n) => Predicate (LengthList n) t where
  validate p x =
    if l == fromIntegral x'
      then Nothing
      else Just ("List's length is not equal to " <> show x' <> ". Length is: " <> show l)
    where
      l = length (toList x) 
      x' = natVal p

-- |
-- A predicate that checks whether or not a given 'Foldable'
-- is empty.
data NonEmpty

instance (Foldable t) => Predicate NonEmpty (t a) where
  validate _ x =
    if null x
      then Just ("Foldable is not nonempty. Size is: " <> show (length x))
      else Nothing

-- |
-- A predicate that checks whether or not a given 'Foldable'
-- is a certain size.
data Size (n :: Nat)

instance (Foldable t, KnownNat n) => Predicate (Size n) (t a) where
  validate p x =
    if length x == fromIntegral x'
      then Nothing
      else Just ("Foldable is not of size " <> show x' <> ". Size is: " <> show (length x))
    where
      x' = natVal p

-- ** Logical
-------------------------

-- |
-- A logical negation of a predicate.
data Not r

instance Predicate r x => Predicate (Not r) x where
  validate _ =
    maybe (Just "A subpredicate didn't fail") (const Nothing) .
    validate (undefined :: r)

-- |
-- A logical conjunction predicate, composed of two other predicates.
data And l r

instance (Predicate l x, Predicate r x) => Predicate (And l r) x where
  validate _ x =
    fmap (showString "The left subpredicate failed with: ") 
         (validate (undefined :: l) x) 
      <|>
    fmap (showString "The right subpredicate failed with: ") 
         (validate (undefined :: r) x)

-- |
-- A logical disjunction predicate, composed of two other predicates.
data Or l r

instance (Predicate l x, Predicate r x) => Predicate (Or l r) x where
  validate _ x =
    case (validate (undefined :: l) x, validate (undefined :: r) x) of
      (Just a, Just b) -> 
        Just $ "Both subpredicates failed. First with: " <> a <> ". Second with: " <> b <> "."
      _ -> 
        Nothing


-- ** Numeric
-------------------------

-- |
-- A predicate, which ensures that a value is less than the specified type-level number.
data LessThan (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (LessThan n) x where
  validate p x =
    if x < fromIntegral x'
      then Nothing
      else Just ("Value is not less than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is greater than the specified type-level number.
data GreaterThan (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (GreaterThan n) x where
  validate p x =
    if x > fromIntegral x'
      then Nothing
      else Just ("Value is not greater than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is greater than or equal to the specified type-level number.
data From (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (From n) x where
  validate p x =
    if x >= fromIntegral x'
      then Nothing
      else Just ("Value is less than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is less than or equal to the specified type-level number.
data To (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (To n) x where
  validate p x =
    if x <= fromIntegral x'
      then Nothing
      else Just ("Value is greater than " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that a value is between or equal to either of the specified type-level numbers.
data FromTo (mn :: Nat) (mx :: Nat)

instance (Ord x, Num x, KnownNat mn, KnownNat mx, mn <= mx) => Predicate (FromTo mn mx) x where
  validate p x =
    if x >= fromIntegral mn' && x <= fromIntegral mx'
      then Nothing
      else Just ("Value is out of range (minimum: " <> show mn' <> ", maximum: " <> show mx' <> ")")
    where
      mn' = natVal (Proxy :: Proxy mn)
      mx' = natVal (Proxy :: Proxy mx)

-- |
-- A predicate, which ensures that a value equals to the specified type-level number.
data EqualTo (n :: Nat)

instance (Ord x, Num x, KnownNat n) => Predicate (EqualTo n) x where
  validate p x =
    if x == fromIntegral x'
      then Nothing
      else Just ("Value does not equal " <> show x')
    where
      x' = natVal p

-- |
-- A predicate, which ensures that the value is greater than zero.
type Positive =
  GreaterThan 0

-- |
-- A predicate, which ensures that the value is less than or equal to zero.
type NonPositive =
  To 0

-- |
-- A predicate, which ensures that the value is less than zero.
type Negative = 
  LessThan 0

-- |
-- A predicate, which ensures that the value is greater than or equal to zero.
type NonNegative =
  From 0

-- |
-- A range of values from zero to one, including both.
type ZeroToOne =
  FromTo 0 1

-- |
-- A typeclass containing "safe" conversions between refined predicates
-- where the target is /weaker/ than the source: that is, all values that
-- satisfy the first predicate will be guaranteed to satisfy the second.
--
-- Take care: writing an instance declaration for your custom predicates
-- is the same as an assertion that 'weaken' is safe to use.
--
-- For most instances, explicit type annotations for the result
-- value's type might be required.
class Weaken from to where
  weaken :: Refined from x -> Refined to x
  weaken = coerce

instance Weaken (And l r) l
instance Weaken (And l r) r
instance Weaken l (Or l r)
instance Weaken r (Or l r)
instance (n <= m) => Weaken (LessThan n) (LessThan m)
instance (n <= m) => Weaken (LessThan n) (To m)
instance (n <= m) => Weaken (To n) (To m)
instance (m <= n) => Weaken (GreaterThan n) (GreaterThan m)
instance (m <= n) => Weaken (GreaterThan n) (From m)
instance (m <= n) => Weaken (From n) (From m)
instance (p <= n, m <= q) => Weaken (FromTo n m) (FromTo p q)
instance (p <= n) => Weaken (FromTo n m) (From p)
instance (m <= q) => Weaken (FromTo n m) (To q)

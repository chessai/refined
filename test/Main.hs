{-# language DataKinds #-}
{-# language TemplateHaskell #-}

module Main (main) where

import Refined
import Control.Monad (guard)
import Data.List (replicate)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

baz :: Refined NonEmpty (Set Int)
baz = $$(refineTH (Set.singleton 0))

-- this will fail to compile
--baz' :: Refined NonEmpty (Set Int)
--baz' = $$(refineTH (Set.empty))

bar :: Refined NonEmpty (Map Int Int)
bar = $$(refineTH (Map.singleton 0 1))

-- this will fail to compile
--bar' :: Refined NonEmpty (Map Int Int)
--bar' = $$(refineTH (Map.empty))

foo :: [Bool]
foo = foldl1 (\x acc -> x >>= guard >> acc)
  ($$(refineTH (replicate 3 True)) :: Refined NonEmpty [Bool])

-- this will fail to compile
--foo' :: [Bool]
--foo' = foldl1 (\x acc -> x >>= guard >> acc)
--  ($$(refineTH (replicate 0 True)) :: Refined NonEmpty [Bool])

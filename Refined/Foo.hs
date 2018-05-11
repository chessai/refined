{-# language DataKinds #-}
{-# language TemplateHaskell #-}

{-| A good use of Refined, with foldl1, which
 - cannot handle empty lists.
-}

import Refined
import Control.Monad (guard)
import Data.List (replicate)

import Data.Map (Map)
import qualified Data.Map as Map

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

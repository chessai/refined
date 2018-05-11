{-# language DataKinds #-}
{-# language TemplateHaskell #-}

{-| A good use of Refined, with foldl1, which
 - cannot handle empty lists.
-}

import Refined
import Control.Monad (guard)
import Data.List (replicate)

foo :: [Bool]
foo = foldl1 (\x acc -> x >>= guard >> acc)
  ($$(refineTH (replicate 3 True)) :: Refined NonEmpty [Bool])

-- this will fail to compile
--foo' :: [Bool]
--foo' = foldl1 (\x acc -> x >>= guard >> acc)
--  ($$(refineTH (replicate 0 True)) :: Refined NonEmpty [Bool])

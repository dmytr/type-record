{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Examples.DistinctFieldNames where

import           Data.Type.Record
import           GHC.Generics

data Test1 = Test1
  { _a :: Int
  , _b :: Bool
  } deriving (Generic)

data Test2 = Test2
  { _c :: Char
  , _d :: Bool
  } deriving (Generic)

data Test3 = Test3
  { _b :: Bool
  , _d :: Int
  } deriving (Generic)

test :: (DistinctFieldNames a b) => a -> b -> Bool
test _ _ = True


-- Compiles:
--
a = test (Test1 1 True) (Test2 'a' False)


-- Does not compile:
--
-- b = test (Test1 1 True) (Test3 False 5)
--
-- With error: "Types Test1 and Test3 have common fields: '["_b"]"

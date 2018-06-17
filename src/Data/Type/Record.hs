{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Record where

import           Data.Kind
import           Data.Singletons.Prelude.List
import           Data.Type.Bool
import           GHC.Generics
import           GHC.TypeLits

type family RecordFields (r :: * -> *) :: [(Symbol, *)] where
  RecordFields (l :*: r)                                   = Union (RecordFields l) (RecordFields r)
  RecordFields (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a)) = '[ '(name, a) ]
  RecordFields (M1 _ m a)                                  = RecordFields a
  RecordFields _                                           = '[]

type family RecordFieldNames (r :: * -> *) :: [Symbol] where
  RecordFieldNames (l :*: r)                                   = Union (RecordFieldNames l) (RecordFieldNames r)
  RecordFieldNames (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a)) = '[ name ]
  RecordFieldNames (M1 _ m a)                                  = RecordFieldNames a
  RecordFieldNames U1                                          = '[]

type family DistinctFieldNames a b :: Constraint where
  DistinctFieldNames a b = If (Null (Intersect (RecordFieldNames (Rep a)) (RecordFieldNames (Rep b))))
                              (() :: Constraint)
                              (TypeError ('Text "Types " ':<>: 'ShowType a ':<>:
                                          'Text " and " ':<>: 'ShowType b ':<>:
                                          'Text " have common fields: " ':<>:
                                          'ShowType (Intersect (RecordFieldNames (Rep a)) (RecordFieldNames (Rep b)))))

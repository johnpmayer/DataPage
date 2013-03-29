
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, TypeFamilies,
    GADTs, FlexibleInstances, RankNTypes, TypeOperators #-}

module Object where

import ByteNat

import Data.Int
import Data.Singletons
import Foreign.Ptr
import Foreign.Storable

data FiniteSet :: Nat -> * where
    FZero :: FiniteSet ('Succ n)
    FSucc :: FiniteSet n -> FiniteSet ('Succ n)

$(singletons [d|

    data U = INT | BOOL | CHAR
        deriving (Eq) 

    |])

type family DBType (u :: U) :: *
type instance DBType INT = Int64
type instance DBType BOOL = Bool
type instance DBType CHAR = Char

$(singletons [d|

    data ADTChar = CA|CB|CC|CD

    data ColumnDef = Attr [ADTChar] U

    data TableDef = Sch Nat [ColumnDef]

    |])

data SomeADTChar = forall (c :: ADTChar). SomeADTChar (SADTChar c)

cnameFromInt :: Int8 -> SomeADTChar
cnameFromInt 0 = SomeADTChar SCA
cnameFromInt 1 = SomeADTChar SCB
cnameFromInt 2 = SomeADTChar SCC
cnameFromInt 3 = SomeADTChar SCD
cnameFromInt _ = undefined

intFromADTChar :: SomeADTChar -> Int8
intFromADTChar (SomeADTChar SCA) = 0
intFromADTChar (SomeADTChar SCB) = 1
intFromADTChar (SomeADTChar SCC) = 2
intFromADTChar (SomeADTChar SCD) = 3

instance Storable SomeADTChar where
    sizeOf _ = sizeOf (0 :: Int8)
    alignment _ = alignment (0 :: Int8)
    peek ptr = fmap cnameFromInt (peek (castPtr ptr))
    poke ptr a = poke (castPtr ptr) (intFromADTChar a)

data SomeSTableDef = forall (s :: TableDef). SomeSTableDef (STableDef s)

data Row :: Nat -> TableDef -> * where
    EmptyRow :: Row 'Zero ('Sch 'Zero '[])
    ConsRow :: DBType u -> Row n ('Sch n s) 
            -> Row ('Succ n) ('Sch ('Succ n) (('Attr name u) ': s))

data SomeRow = forall (t :: TableDef). SomeRow (Row MaxNColumns t)

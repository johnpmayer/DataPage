
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, TypeFamilies,
    GADTs, FlexibleInstances, RankNTypes, TypeOperators #-}

module Object where

import ByteNat
import Schema

import Data.Int
import Data.Singletons
import Foreign.Ptr
import Foreign.Storable

data FiniteSet :: Nat -> * where
    FZero :: FiniteSet ('Succ n)
    FSucc :: FiniteSet n -> FiniteSet ('Succ n)

data SomeAChar = forall (c :: AChar). SomeAChar (SAChar c)

cnameFromInt :: Int8 -> SomeAChar
cnameFromInt 0 = SomeAChar SCA
cnameFromInt 1 = SomeAChar SCB
cnameFromInt 2 = SomeAChar SCC
cnameFromInt 3 = SomeAChar SCD
cnameFromInt _ = undefined

intFromAChar :: SomeAChar -> Int8
intFromAChar (SomeAChar SCA) = 0
intFromAChar (SomeAChar SCB) = 1
intFromAChar (SomeAChar SCC) = 2
intFromAChar (SomeAChar SCD) = 3

instance Storable SomeAChar where
    sizeOf _ = sizeOf (0 :: Int8)
    alignment _ = alignment (0 :: Int8)
    peek ptr = fmap cnameFromInt (peek (castPtr ptr))
    poke ptr a = poke (castPtr ptr) (intFromAChar a)

data SomeSSchema = forall (s :: Schema). SomeSSchema (SSchema s)

data Row :: Nat -> Schema -> * where
    EmptyRow :: Row 'Zero ('Sch 'Zero '[])
    ConsRow :: Store u -> Row n ('Sch n s) 
            -> Row ('Succ n) ('Sch ('Succ n) (('Attr name u) ': s))

data SomeRow = forall (t :: Schema). SomeRow (Row MaxNColumns t)

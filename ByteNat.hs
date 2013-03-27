
{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes,
    FlexibleInstances, PolyKinds, ScopedTypeVariables #-}

module ByteNat where

import Data.Int
import Foreign.Ptr
import Foreign.Storable

data Nat = Zero | Succ Nat

data ByteNat :: Nat -> * where
  BN0 :: ByteNat Zero
  BN1 :: ByteNat (Succ Zero)
  BN2 :: ByteNat (Succ (Succ Zero))
  BN3 :: ByteNat (Succ (Succ (Succ Zero)))

-- we would like an instance of Storable for ByteNat
-- this fails because ByteNat :: Nat -> *
-- the class Storable expects types :: *

{-
 - What do we want to express?
 -
 - Give me an Int, and I'll give you a (ByteNat t) for some (t :: Nat)
 -}

data SomeByteNat = forall (a :: Nat). SomeByteNat (ByteNat a)

repFromInt :: Int8 -> SomeByteNat
repFromInt 0 = SomeByteNat BN0
repFromInt 1 = SomeByteNat BN1
repFromInt 2 = SomeByteNat BN2
repFromInt 3 = SomeByteNat BN3
repFromInt _ = error "Out of range"

intFromRep :: SomeByteNat -> Int8
intFromRep (SomeByteNat BN0) = 0
intFromRep (SomeByteNat BN1) = 1
intFromRep (SomeByteNat BN2) = 2
intFromRep (SomeByteNat BN3) = 3

instance Storable SomeByteNat where
    sizeOf _ = sizeOf (0 :: Int8)
    alignment _ = alignment (0 :: Int8)
    peek ptr = fmap repFromInt (peek (castPtr ptr))
    poke ptr a = poke (castPtr ptr) (intFromRep a)



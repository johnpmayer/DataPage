
{-# OPTIONS -Wall #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, RankNTypes,
    FlexibleInstances, PolyKinds, ScopedTypeVariables,
    TemplateHaskell, TypeFamilies #-}

module ByteNat where

import Data.Int
import Data.Singletons
import Foreign.Ptr
import Foreign.Storable

$(singletons [d| 

    data Nat = Zero | Succ Nat 
   
    {-
    lt :: Nat -> Nat -> Bool
    lt Zero Zero = False
    lt _ Zero = False
    lt Zero _ = True
    -}
    
    |])

type N0 = 'Zero
type N1 = 'Succ N0
type N2 = 'Succ N1
type N3 = 'Succ N2
type N4 = 'Succ N3

type MaxNColumns = N4

data ByteNat :: Nat -> * where
  BN0 :: ByteNat N0
  BN1 :: ByteNat N1
  BN2 :: ByteNat N2
  BN3 :: ByteNat N3

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



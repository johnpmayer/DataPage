
{-# OPTIONS -Wall #-}

{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, TypeFamilies,
    GADTs, PolyKinds, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

module Schema where

import Data.Int
import Data.Reflection
import Data.Singletons
import Foreign.Storable

$(singletons [d|
    
    data Nat = Succ Nat | Zero deriving (Eq, Ord)
    
    |])

$(singletons [d|

    data U = INTEGER | VARCHAR Nat

    data AChar  = CA | CB | CC | CD | CE | CF | CG | CH | CI
                | CJ | CK | CL | CM | CN | CO | CP | CQ | CR
                | CS | CT | CU | CV | CW | CX | CY | CZ
        deriving (Read, Show, Eq)
    
    data Attribute = Attr [AChar] U
    
    data Schema = Sch [Attribute]
    
    |])

data Vec :: * -> Nat -> * where
    VNil :: Vec a 'Zero
    VCons :: a -> Vec a n -> Vec a ('Succ n)

type Varchar n = Vec Char n

type family Store (u :: U) :: *
type instance Store INTEGER = Int64
type instance Store (VARCHAR n) = Varchar n



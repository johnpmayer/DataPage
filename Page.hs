
{-# OPTIONS -Wall #-}

module Page where

import Data.Int
import Foreign.Storable

import ByteNat ()
import Object ()
import Schema ()

type ObjectId = Int64
type PageAddr = Int64

data AllocPage = AllocPage [(ObjectId, PageAddr)]

instance Storable AllocPage where
    sizeOf = undefined
    alignment = undefined
    peek _ = undefined
    poke _ _ = undefined

data Object

data ObjectPage = ObjectPage [Object]

data Row a

data DataPage schema = DataPage [Row schema]


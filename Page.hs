
{-# OPTIONS -Wall #-}

module Page where

import Data.Int
import Foreign.Storable ()

import ByteNat ()
import Object
import Schema ()

type ObjectId = Int64
type PageAddr = Int64

data GlobalMetaPage = GMP
    { freeExtentHead :: PageAddr
    , objectPageHeads :: [(ObjectId, PageAddr, SomeSTableDef)] 
    }

data TablePage n schema = TP
    { freePageHead :: PageAddr
    , prevPage :: Maybe PageAddr
    , nextPage :: Maybe PageAddr
    , rows :: [Row n schema] 
    }

-- instance Storable ... where



{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, KindSignatures, 
    MultiParamTypeClasses, DataKinds, ExistentialQuantification #-}


module Page where

import Data.Word
import Foreign.Ptr
import Foreign.Storable

import ByteNat
import Object
import Schema

type ObjectId = Word64
type PageAddr = Word64

-- Storable Maybe that always just takes up twice as much space
instance Storable a => Storable (Maybe a) where
    sizeOf (Just x) = 2 * sizeOf x
    sizeOf Nothing  = error "Storable Maybe pattern match"
    alignment (Just x) = alignment x
    alignment Nothing  = error "Storable Maybe pattern match"
    -- peek :: Ptr (Maybe a) -> IO (Maybe a)
    peek p = do 
        (i :: Word8) <- peek (castPtr p)
        if i == 0
        then return Nothing
        else fmap Just $ peekElemOff ((castPtr p) :: Ptr a) 1
    -- poke :: Ptr (Maybe a) -> (Maybe a) -> IO (Maybe a)
    poke p (Nothing) = do 
        poke (castPtr p) (0 :: Word8)
    poke p (Just x) = do 
        poke (castPtr p) (1 :: Word8)
        pokeElemOff (castPtr p) 1 x

data TableInfo = forall (n :: Nat) (schema :: Schema). TI
    { object :: ObjectId
    , start :: PageAddr
    , nCols :: ByteNat n
    , tableDef :: SSchema schema }

instance Storable TableInfo where

data GlobalMetaPage = GMP
    { freeExtentHead :: Maybe PageAddr
    , extentBreak :: PageAddr
    , objects :: [TableInfo]
    }

instance Storable GlobalMetaPage where
    sizeOf _ = 4096
    alignment _ = 4096
    peek ptr = undefined
    poke ptr gmp = undefined
        
data TablePage = forall (n :: Nat) (schema :: Schema). TP
    { freePageHead :: Maybe PageAddr
    , prevPage :: Maybe PageAddr
    , nextPage :: Maybe PageAddr
    , rows :: [Row n schema] 
    }

instance Storable TablePage where
    sizeOf _ = 4096
    alignment _ = 4096
    peek ptr = undefined
    poke ptr gmp = undefined


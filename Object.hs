
{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, KindSignatures, DataKinds, TypeFamilies,
    GADTs, FlexibleInstances, RankNTypes, TypeOperators #-}

module Object where

import Data.Int
import Data.Singletons

$(singletons [d|

    data U = INT | BOOL | CHAR
        deriving (Eq) 
    
    |])

type family DBType (u :: U) :: *
type instance DBType INT = Int64
type instance DBType BOOL = Bool
type instance DBType CHAR = Char

$(singletons [d|

    data CName = CA|CB|CC|CD

    data Attribute = Attr [CName] U

    data Schema = Sch [Attribute]

    |])

data Row :: Schema -> * where
    EmptyRow :: Row (Sch '[])
    ConsRow :: DBType u -> Row ('Sch s) -> Row (Sch (('Attr name u) ': s))

data SomeSSchema = forall (s :: Schema). SomeSSchema (SSchema s)



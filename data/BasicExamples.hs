{-# LANGUAGE DeriveAnyClass #-}

module BasicExamples where

import           Internal.Intermediate.Bridge.Generics
import           Data.Text       (Text)
import           GHC.Generics

newtype SimpleRecord =
  SimpleRecord
    {f1 :: Int
    } deriving (Generic, BridgeType)

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, BridgeType)

newtype SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)

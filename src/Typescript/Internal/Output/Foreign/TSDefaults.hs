{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Typescript.Internal.Output.Foreign.TSDefaults where

import           Data.Text
import qualified Data.Text                                as T

import           Typescript.Internal.Intermediate.Lang
import           Typescript.Internal.Output.Foreign.Class

{- DEFAULT FOREIGN INSTANCES -}
instance IsForeignType (TSComposite f)
    => IsForeignType (TSIntermediate f) where
    toForeignType (TSPrimitiveType prim) = toForeignType prim
    toForeignType (TSCompositeType composite) = toForeignType composite

instance IsForeignType TSPrimitive where
    toForeignType TSString = ForeignType "string" Nothing
    toForeignType TSNumber = ForeignType "number" Nothing
    toForeignType TSBoolean = ForeignType "boolean" Nothing
    toForeignType TSVoid = ForeignType "void" Nothing

showField :: (IsForeignType (TSIntermediate f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) = fName <> " : "
    <> (refName . toForeignType) fType

showFields :: (IsForeignType (TSIntermediate f)) => Text -> [TSField f] -> Text
showFields term fields = T.intercalate "\n" $
    fmap (\f -> "  " <> showField f <> term) fields

defaultForeignArray :: (IsForeignType (TSIntermediate f))
    => TSCollection f -> ForeignType
defaultForeignArray (TSCollection tsType') =
    ForeignType ("Array<" <> rep <> ">") Nothing
  where
    rep = refName . toForeignType $ tsType'

defaultForeignUnion :: (IsForeignType (TSIntermediate f))
    => Text -> TSUnion f -> ForeignType
defaultForeignUnion unionName (TSUnion tsTypes') =
    ForeignType { refName     = unionName
                , declaration = Just $ "type " <> unionName <> " = " <> ns
                }
  where
    ns = intercalate " | " $ fmap (refName . toForeignType) tsTypes'

defaultOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
defaultOption (TSOption tsType') =
    ForeignType ((refName . toForeignType $ tsType') <> " | null ") Nothing

mkTSInterface :: (IsForeignType (TSIntermediate f))
    => Text -> TSRecord f -> ForeignType
mkTSInterface iName (TSRecord fields') =
    ForeignType { refName     = iName
                , declaration = Just $ "interface " <> iName <> " { \n"
                      <> showFields ";" fields' <> "\n}"
                }


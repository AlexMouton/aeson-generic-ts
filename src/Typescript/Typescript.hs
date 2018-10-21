module Typescript.Typescript where

import           Bridge.Intermediate
import           Data.Text

data TSImport =
  TSImport
    {libName :: Text}

printTypescript :: (IsForeignType t) => Maybe [TSImport] -> t -> Text
printTypescript Nothing tsType = (declaration . toForeignType) tsType
printTypescript (Just imports) tsType =
  importStatements <> "\n" <> (declaration . toForeignType) tsType
  where
    importStatements = Data.Text.intercalate "\n" $ fmap printImportStatement imports

printImportStatement :: TSImport -> Text
printImportStatement (TSImport lib) =
  "import * from " <> lib
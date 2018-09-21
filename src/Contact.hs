{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Contact
    ( Contact
    , new
    , name
    , number
    ) where

import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics

data Contact = Contact { name :: Text, number :: Text } deriving (Generic)

instance Yaml.FromJSON Contact
instance Yaml.ToJSON Contact

new :: Text -> Text -> Contact
new name number = Contact {..}
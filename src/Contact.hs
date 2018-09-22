{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module Contact
    ( Contact
    , new
    , name
    , number
    )
where

import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics

data Contact = Contact { name :: Text
                       , number :: Text
                       } deriving (Eq, Generic, Show)

instance Yaml.FromJSON Contact
instance Yaml.ToJSON Contact

new :: Text -> Text -> Either Text Contact
new name number =
    case (name == "", number == "") of
        (True , _    ) -> Left "Name must be present."
        (_    , True ) -> Left "Number must be present."
        (False, False) -> Right Contact {..}
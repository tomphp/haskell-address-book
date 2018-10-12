{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Domain.Contact
    ( Contact
    , new
    , name
    , number
    )
where

import Data.Text (Text)

data Contact = Contact { name :: Text
                       , number :: Text
                       } deriving (Eq, Show)

new :: Text -> Text -> Either Text Contact
new name number =
    case (name == "", number == "") of
        (True , _    ) -> Left "Name must be present."
        (_    , True ) -> Left "Number must be present."
        (False, False) -> Right Contact {..}
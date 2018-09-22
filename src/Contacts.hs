{-# LANGUAGE DeriveGeneric #-}

module Contacts
    ( Contacts
    , new
    , add
    , foreach
    )
where

import Control.Monad (forM_)
import qualified Data.Yaml as Yaml
import GHC.Generics

import Contact (Contact)

newtype Contacts = Contacts [Contact] deriving (Generic)

instance Yaml.FromJSON Contacts
instance Yaml.ToJSON Contacts

new :: Contacts
new = Contacts []

add :: Contact -> Contacts -> Contacts
add contact (Contacts contacts) = Contacts (contact : contacts)

foreach :: Monad m => Contacts -> (Contact -> m a) -> m ()
foreach (Contacts contacts) = forM_ contacts
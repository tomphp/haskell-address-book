{-# LANGUAGE DeriveGeneric #-}
module Contacts
    ( Contacts
    , new
    , asList
    , add
    ) where

import qualified Data.Yaml as Yaml
import GHC.Generics

import Contact (Contact)

newtype Contacts = Contacts [Contact] deriving (Generic)

instance Yaml.FromJSON Contacts
instance Yaml.ToJSON Contacts

new :: Contacts
new = Contacts []

asList :: Contacts -> [Contact]
asList (Contacts contacts) = contacts

add :: Contact -> Contacts -> Contacts
add contact (Contacts contacts) = Contacts (contact : contacts)
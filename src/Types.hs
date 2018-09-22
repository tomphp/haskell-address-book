{-# LANGUAGE DeriveFunctor #-}

module Types where

import qualified Data.Yaml as Yaml
import Contact (Contact)
import Contacts (Contacts)

data Action = ListContacts | AddContact | Quit

data Config = Config { configFile :: String }

type UIInterpreter a = UICommand (IO a) -> IO a

data UICommand next = DisplayWelcomeBanner next
                    | DisplayMessage String next
                    | GetAction (Maybe Action -> next)
                    | DisplayContactList Contacts next
                    | GetContact (Contact -> next)
                    | Exit Int deriving (Functor)

type StorageInterpreter a = StorageCommand (IO a) -> IO a

data StorageCommand next = ReadContacts String (Either Yaml.ParseException Contacts -> next)
                         | WriteContacts String Contacts next deriving (Functor)
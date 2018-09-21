{-# LANGUAGE DeriveFunctor #-}

module Types where

import Control.Monad.Free (Free(..))
import qualified Data.Yaml as Yaml
import Contact (Contact)
import Contacts (Contacts)

data Action = ListContacts | AddContact | Quit

data Config = Config { configFile :: String }

type Interpreter = Free Command () -> IO ()

data Command next = DisplayWelcomeBanner next
                  | DisplayMessage String next
                  | GetAction (Maybe Action -> next)
                  | DisplayContactList Contacts next
                  | GetContact (Contact -> next)
                  | ReadContacts String (Either Yaml.ParseException Contacts -> next)
                  | WriteContacts String Contacts next
                  | Exit Int deriving (Functor)

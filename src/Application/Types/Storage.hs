{-# LANGUAGE DeriveFunctor #-}

module Application.Types.Storage where

import qualified Data.Yaml as Yaml

import Contacts (Contacts)

type Interpreter a = Command (IO a) -> IO a

data Command next = ReadContacts String (Either Yaml.ParseException Contacts -> next)
                  | WriteContacts String Contacts next deriving (Functor)
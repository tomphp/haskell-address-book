{-# LANGUAGE DeriveFunctor #-}

module Application.Types.Storage where

import qualified Data.Yaml as Yaml

import Contacts (Contacts)

type Interpreter a = Command (IO a) -> IO a

data Command next = ReadContacts FilePath (Either Yaml.ParseException Contacts -> next)
                  | WriteContacts FilePath Contacts next deriving (Functor)
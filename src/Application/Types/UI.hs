{-# LANGUAGE DeriveFunctor #-}

module Application.Types.UI where

import Data.Text (Text)

import Contact (Contact)
import Contacts (Contacts)

data Action = ListContacts | AddContact | Quit

type Interpreter a = Command (IO a) -> IO a

data Command next = DisplayWelcomeBanner next
                  | DisplayMessage Text next
                  | GetAction (Maybe Action -> next)
                  | DisplayContactList Contacts next
                  | GetContact (Contact -> next)
                  | Exit Int deriving (Functor)
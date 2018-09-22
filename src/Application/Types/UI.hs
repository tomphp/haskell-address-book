{-# LANGUAGE DeriveFunctor #-}

module Application.Types.UI where

import Data.Text (Text)

import Contact (Contact)
import Contacts (Contacts)

data Choice = Yes | No

data Action = ListContacts | AddContact | Save | Quit

type Interpreter a = Command (IO a) -> IO a

data Command next = DisplayWelcomeBanner next
                  | DisplayMessage Text next
                  | DisplayContactList Contacts next
                  | GetChoice Text (Choice -> next)
                  | GetAction (Action -> next)
                  | GetContact (Contact -> next)
                  | Exit Int deriving (Functor)
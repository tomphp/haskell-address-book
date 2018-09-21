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
                  | DisplayCommandList next
                  | ReadContacts String (Either Yaml.ParseException Contacts -> next)
                  | WriteContacts String Contacts next
                  | Exit Int

instance Functor Command where
    fmap f (DisplayWelcomeBanner next)        = DisplayWelcomeBanner (f next)
    fmap f (DisplayMessage msg   next)        = DisplayMessage msg (f next)
    fmap f (GetAction fnext)                  = GetAction (f . fnext)
    fmap f (DisplayContactList contacts next) = DisplayContactList contacts (f next)
    fmap f (GetContact fnext)                 = GetContact (f . fnext)
    fmap f (DisplayCommandList next)          = DisplayCommandList (f next)
    fmap f (ReadContacts path fnext)          = ReadContacts path (f . fnext)
    fmap f (WriteContacts path contacts next) = WriteContacts path contacts (f next)
    fmap _ (Exit num)                         = Exit num
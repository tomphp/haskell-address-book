module Domain.Contacts
    ( Contacts
    , new
    , add
    , all
    , foreach
    )
where

import Control.Monad (forM_)
import Prelude       (Monad)

import Domain.Contact (Contact)

newtype Contacts = Contacts [Contact]

new :: Contacts
new = Contacts []

add :: Contact -> Contacts -> Contacts
add contact (Contacts contacts) = Contacts (contact : contacts)

all :: Contacts -> [Contact]
all (Contacts contacts) = contacts

foreach :: Monad m => Contacts -> (Contact -> m a) -> m ()
foreach (Contacts contacts) = forM_ contacts

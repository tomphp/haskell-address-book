module Domain.State
    ( State
    , new
    , getContacts
    , setContacts
    , hasUnsaved
    , setSaved
    , setUnsaved
    )
where

import qualified Domain.Contacts as Contacts

data State = State { getContacts :: Contacts.Contacts
                   , hasUnsaved  :: Bool
                   }

new :: State
new = State { getContacts = Contacts.new, hasUnsaved = False }

setUnsaved :: State -> State
setUnsaved state = state { hasUnsaved = True }

setSaved :: State -> State
setSaved state = state { hasUnsaved = False }

setContacts :: Contacts.Contacts -> State -> State
setContacts contacts state = state { getContacts = contacts }
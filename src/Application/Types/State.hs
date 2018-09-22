module Application.Types.State
    ( State
    , new
    , getContacts
    , setContacts
    , hasUnsaved
    , setSaved
    , setUnsaved
    ) where

import Contacts (Contacts)

import qualified Contacts

data State = State { getContacts :: Contacts
                   , hasUnsaved  :: Bool
                   }

new :: State
new = State { getContacts = Contacts.new, hasUnsaved = False }

setUnsaved :: State -> State
setUnsaved state = state { hasUnsaved = True }

setSaved :: State -> State
setSaved state = state { hasUnsaved = False }

setContacts :: Contacts -> State -> State
setContacts contacts state = state { getContacts = contacts }
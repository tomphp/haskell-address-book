{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.MainLogic (main) where

import Control.Monad (forever, when)

import qualified Domain.Action      as Action
import qualified Domain.Application as App
import qualified Domain.Choice      as Choice
import qualified Domain.Contacts    as Contacts

type Application m = (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m)

main :: (Application m) => m ()
main = do
    App.displayWelcomeBanner
    loadContacts
    forever mainLoop

loadContacts :: Application m => m ()
loadContacts =
    App.readContacts >>= either quitWithMessage App.putContacts
  where
    quitWithMessage message = App.displayMessage message >> App.exit 1

mainLoop :: Application m => m ()
mainLoop = App.getAction >>= doAction

doAction :: Application m => Action.Action -> m ()
doAction Action.ListContacts = displayContactList
doAction Action.AddContact   = addContact
doAction Action.Save         = saveContacts
doAction Action.Quit         = quit

displayContactList :: (App.State m, App.UI m) => m ()
displayContactList = App.getContacts >>= App.displayContactList

addContact :: (App.UI m, App.State m) => m ()
addContact = do
    contacts <- App.getContacts
    contact  <- App.getContact

    App.putContacts (Contacts.add contact contacts)
    App.setUnsaved

saveContacts :: Application m => m ()
saveContacts = do
    contacts <- App.getContacts
    App.writeContacts contacts
    App.setSaved
    App.displayMessage "Contacts saved."

quit :: Application m => m ()
quit = do
    unsaved <- App.hasUnsaved
    when unsaved promptForSave
    App.exit 0

promptForSave :: Application m => m ()
promptForSave = do
    save <- App.getChoice "Do you want to save changes?"
    when (Choice.isYes save) saveContacts


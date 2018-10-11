{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.MainLogic (main) where

import Control.Monad (forever, when)

import qualified Domain.Application as App
import qualified Domain.Contacts    as Contacts
import qualified Domain.Action      as Action
import qualified Domain.Choice      as Choice

main :: (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m) => m ()
main = do
    App.displayWelcomeBanner
    loadContacts
    forever mainLoop

loadContacts :: (App.Config m, App.State m, App.Storage m, App.UI m) => m ()
loadContacts = do
    contacts <- App.readContacts
    case contacts of
        Right cs  -> App.putContacts cs
        Left  err -> App.displayMessage err >> App.exit 1

mainLoop :: (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m) => m ()
mainLoop = App.getAction >>= doAction

doAction :: (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m) => Action.Action -> m ()
doAction =
    \case
        Action.ListContacts -> displayContactList
        Action.AddContact   -> addContact
        Action.Save         -> saveContacts
        Action.Quit         -> quit

displayContactList :: (App.State m, App.UI m) => m ()
displayContactList = App.getContacts >>= App.displayContactList

addContact :: (App.UI m, App.State m) => m ()
addContact = do
    contacts <- App.getContacts
    contact  <- App.getContact

    App.putContacts (Contacts.add contact contacts)
    App.setUnsaved

saveContacts :: (App.Config m, App.State m, App.Storage m, App.UI m) => m ()
saveContacts = do
    contacts <- App.getContacts
    App.writeContacts contacts
    App.setSaved
    App.displayMessage "Contacts saved."

quit :: (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m) => m ()
quit = do
    unsaved <- App.hasUnsaved

    when unsaved promptForSave

    App.exit 0

promptForSave :: (Applicative m, App.Config m, App.State m, App.Storage m, App.UI m) => m ()
promptForSave = do
    save <- App.getChoice "Do you want to save changes?"

    when (Choice.isYes save) saveContacts


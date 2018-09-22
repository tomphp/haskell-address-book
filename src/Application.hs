{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Application
    ( module App
    , module Application.Types.Base
    , main
    )
where

import Control.Monad (forever)

import Application.Types.Base
import Application.Types.UI (Action(..))
import Application.Commands.Base as App

import qualified Application.Commands.Storage as Storage
import qualified Application.Commands.UI as UI

import qualified Contacts

main :: Application ()
main = do
    UI.displayWelcomeBanner
    Storage.readContacts

    forever mainLoop

mainLoop :: Application ()
mainLoop = UI.getAction >>= doAction

doAction :: Action -> Application ()
doAction =
    \case
        ListContacts -> UI.displayContactList
        AddContact   -> addContact
        Save         -> saveContacts
        Quit         -> quit

addContact :: Application ()
addContact = do
    contacts <- App.getContacts
    contact  <- UI.getContact

    App.putContacts (Contacts.add contact contacts)
    App.setUnsaved

saveContacts :: Application ()
saveContacts = do
    Storage.writeContacts
    App.setSaved
    UI.displayMessage "Contacts saved."

quit :: Application ()
quit = do
    unsaved <- App.hasUnsaved

    if unsaved
        then UI.displayMessage "Please save before quitting."
        else UI.exit 0
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Application
    ( module App
    , module Storage
    , module UI
    , module Application.Types.Base
    , main
    ) where

import Control.Monad (forever)

import qualified Data.Text as T

import Application.Types.Base
import Application.Types.UI (Action(..))
import Application.Commands.Base as App
import Application.Commands.Storage as Storage
import Application.Commands.UI as UI

import qualified Contacts

main :: Application ()
main = do
    UI.displayWelcomeBanner

    contacts <- Storage.readContacts

    case contacts of
        Right cs  -> App.putContacts cs
        Left err  -> do UI.displayMessage (T.pack (show err))
                        UI.exit 1

    forever mainLoop

mainLoop :: Application ()
mainLoop = do
    action <- UI.getAction

    case action of
       Just a  -> doAction a
       Nothing -> UI.displayMessage "ERROR: Bad command"

doAction :: Action -> Application ()
doAction =
    \case
        ListContacts -> UI.displayContactList
        AddContact   -> addContact
        Save         -> saveContacts
        Quit         -> UI.exit 0

addContact :: Application ()
addContact = do
    contacts <- App.getContacts
    contact  <- UI.getContact

    App.putContacts (Contacts.add contact contacts)

saveContacts :: Application ()
saveContacts = do
    Storage.writeContacts
    UI.displayMessage "Contacts saved."
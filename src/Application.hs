{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Application
    ( module Application.Commands.Base
    , module Application.Commands.Storage
    , module Application.Commands.UI
    , module Application.Types.Base
    , main
    ) where

import Control.Monad (forever)

import Application.Types.Base
import Application.Types.UI (Action(..))
import Application.Commands.Base
import Application.Commands.Storage
import Application.Commands.UI

import qualified Contacts

main :: Application ()
main = do
    printWelcomeBanner

    contacts <- readContacts

    case contacts of
        Right cs  -> putContacts cs
        Left err  -> do printMessage (show err)
                        exit 1

    forever mainLoop

mainLoop :: Application ()
mainLoop = do
    action <- getAction

    case action of
       Just a  -> doAction a
       Nothing -> printMessage "ERROR: Bad command"

doAction :: Action -> Application ()
doAction =
    \case
        ListContacts -> listContacts
        AddContact   -> addContact
        Quit         -> exit 0

addContact :: Application ()
addContact = do
    contacts <- getContacts
    contact  <- getContact

    putContacts (Contacts.add contact contacts)

    writeContacts
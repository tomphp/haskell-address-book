{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import System.Environment (getArgs)

import qualified Contacts

import qualified Application
import Application (Application, runApplication)
import Types
import qualified UI

-- Main

main :: IO ()
main = do
    putStrLn ""
    config  <- loadConfig
    runApplication UI.interpret config Contacts.new application

application :: Application ()
application = do
    Application.printWelcomeBanner

    contacts <- Application.readContacts

    case contacts of
        Right cs  -> Application.putContacts cs
        Left err  -> do Application.printMessage (show err)
                        Application.exit 1

    forever mainLoop

mainLoop :: Application ()
mainLoop = do
    Application.printCommandList

    action <- Application.getAction

    case action of
       Just a  -> doAction a
       Nothing -> Application.printMessage "Bad command"

doAction :: Action -> Application ()
doAction =
    \case
        ListContacts -> Application.listContacts
        AddContact   -> addContact
        Quit         -> Application.exit 0

addContact :: Application ()
addContact = do
    contacts <- Application.getContacts
    contact  <- Application.getContact

    Application.putContacts (Contacts.add contact contacts)

    Application.writeContacts

loadConfig :: IO Config
loadConfig = do
    args <- getArgs

    return Config { configFile = head args }
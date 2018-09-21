{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import System.Exit (exitSuccess, exitFailure)
import System.Environment (getArgs)

import qualified Contacts

import qualified Application
import Application (Application, runApplication)
import Types

-- Main

main :: IO ()
main = do
    config  <- loadConfig
    runApplication config Contacts.new application

application :: Application ()
application = do
    Application.printWelcomeBanner

    contacts <- Application.readContacts

    case contacts of
        Right cs  -> Application.putContacts cs
        Left err  -> do liftIO $ print err
                        liftIO exitFailure

    forever mainLoop

mainLoop :: Application ()
mainLoop = do
    Application.printCommandList

    action <- Application.getAction

    case action of
       Just a  -> doAction a
       Nothing -> liftIO $ putStrLn "Bad command"

doAction :: Action -> Application ()
doAction =
    \case
        ListContacts -> Application.listContacts
        AddContact   -> addContact
        Quit         -> liftIO exitSuccess

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
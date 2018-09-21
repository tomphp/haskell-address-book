{-# LANGUAGE LambdaCase, DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Monad (forever, forM_)
import Data.ByteString.Char8 as C8 (pack, unpack, ByteString)
import qualified Data.Yaml as Yaml
import Data.Text as T (Text, pack, unpack)
import GHC.Generics
import System.Exit (exitSuccess)
import System.Environment (getArgs)

import Control.Monad.Reader (ReaderT, ask, runReaderT, lift)

import qualified Contact
import Contact (Contact)
import qualified Contacts
import Contacts (Contacts)

data Action = ListContacts | AddContact | Quit

data Config = Config { configFile :: String }

type Application = ReaderT Config IO ()

actionFromString :: String -> Maybe Action
actionFromString =
    \case
        "l" -> Just ListContacts
        "a" -> Just AddContact
        "q" -> Just Quit
        _   -> Nothing

getAction :: IO (Maybe Action)
getAction =  actionFromString <$> getLine

printContact :: Contact -> IO ()
printContact contact = do
    putStrLn $ "Name:   " ++ (T.unpack $ Contact.name contact)
    putStrLn $ "Number: " ++ (T.unpack $ Contact.number contact)
    putStrLn " ---"

listContacts :: Contacts -> IO ()
listContacts contacts = do
    forM_ (Contacts.asList contacts) printContact

getContact :: IO Contact
getContact = do
    putStrLn "Enter Name:"
    name <- T.pack <$> getLine

    putStrLn "Enter Number:"
    number <- T.pack <$> getLine

    return $ Contact.new name number

writeContacts :: Contacts -> Application
writeContacts contacts = do
    config <- ask
    let content = C8.unpack $ Yaml.encode contacts
    lift $ writeFile (configFile config) content

addContact :: Contacts -> Application
addContact contacts = do
    contact <- lift getContact

    writeContacts (Contacts.add contact contacts)

    lift $ putStrLn "saved"

doAction :: Contacts -> Action -> Application
doAction contacts =
    \case
        ListContacts -> lift $ listContacts contacts
        AddContact   -> addContact contacts
        Quit         -> lift exitSuccess

readContacts :: Config -> IO (Either Yaml.ParseException Contacts)
readContacts config = do
    contents <- readFile $ configFile config

    return $ Yaml.decodeEither' $ C8.pack contents

getConfig = do
    args <- getArgs

    let configFile = head args

    return $ Config { configFile = configFile }

main :: IO ()
main = do
    putStrLn "=== Phone Book ==="

    config  <- getConfig
    decoded <- readContacts config

    case decoded of
        Right contacts -> forever $ mainLoop config contacts
        Left err       -> putStrLn $ show err

printCommandList :: IO ()
printCommandList = do
    putStrLn "Commands:"
    putStrLn "  l  List contacts"
    putStrLn "  a  Add contact"
    putStrLn "  q  Add contact"

mainLoop :: Config -> Contacts -> IO ()
mainLoop config contacts = do
    printCommandList

    action <- getAction

    case action of
       Just a  -> runReaderT (doAction contacts a) config
       Nothing -> putStrLn "Bad command"
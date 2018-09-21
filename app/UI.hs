{-# LANGUAGE LambdaCase #-}

module UI where

import Types

import qualified Data.Text as T

import Contacts (Contacts)
import Contact (Contact)

import qualified Contacts
import qualified Contact

printWelcomeBanner :: IO ()
printWelcomeBanner = putStrLn "=== Phone Book ==="

getAction :: IO (Maybe Action)
getAction =  actionFromString <$> getLine

actionFromString :: String -> Maybe Action
actionFromString =
    \case
        "l" -> Just ListContacts
        "a" -> Just AddContact
        "q" -> Just Quit
        _   -> Nothing

listContacts :: Contacts -> IO ()
listContacts contacts =
    Contacts.foreach contacts printContact

printContact :: Contact -> IO ()
printContact contact = do
    putStrLn $ "Name:   " ++ T.unpack (Contact.name contact)
    putStrLn $ "Number: " ++ T.unpack (Contact.number contact)
    putStrLn "---"

getContact :: IO Contact
getContact = do
    putStrLn "Enter Name:"
    name <- T.pack <$> getLine

    putStrLn "Enter Number:"
    number <- T.pack <$> getLine

    return $ Contact.new name number

printCommandList :: IO ()
printCommandList = do
    putStrLn "Commands:"
    putStrLn "  l  List contacts"
    putStrLn "  a  Add contact"
    putStrLn "  q  Add contact"


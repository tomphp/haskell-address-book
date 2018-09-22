{-# LANGUAGE LambdaCase #-}

module UI where

import qualified Data.Text as T
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)

import Application.Types.UI
import Contacts (Contacts)
import Contact (Contact)

import qualified Contacts
import qualified Contact

interpret :: Interpreter ()
interpret (DisplayWelcomeBanner        x) = UI.printWelcomeBanner >> x
interpret (DisplayMessage msg          x) = UI.printMessage msg >> x
interpret (GetAction                   x) = UI.getAction >>= x
interpret (DisplayContactList contacts x) = UI.listContacts contacts >> x
interpret (GetContact                  x) = UI.getContact >>= x
interpret (Exit code)                     = exit code

printWelcomeBanner :: IO ()
printWelcomeBanner = do
    putStrLn "=================================================================="
    putStrLn "=== Welcome to the Haskell Phone Book                          ==="
    putStrLn "=================================================================="

printMessage :: String -> IO ()
printMessage msg = putStrLn $ ">>> " ++ msg

getAction :: IO (Maybe Action)
getAction = do
    printCommandList
    actionFromString <$> getLine

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
    putStrLn "Enter Name:  "
    name <- T.pack <$> getLine

    putStrLn "Enter Number:"
    number <- T.pack <$> getLine

    return $ Contact.new name number

printCommandList :: IO ()
printCommandList = do
    putStrLn "+-|Commands|-----------------------------------------------------+"
    putStrLn "| l  List contacts                                               |"
    putStrLn "| a  Add contact                                                 |"
    putStrLn "| q  Add contact                                                 |"
    putStrLn "+----------------------------------------------------------------+"

exit :: Int -> IO ()
exit code = do
    putStrLn "Exiting"
    case code of
         0 -> exitSuccess
         _ -> exitWith (ExitFailure code)


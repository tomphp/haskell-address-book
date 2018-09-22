{-# LANGUAGE LambdaCase #-}

module UI where

import Control.Monad.Free (Free(..))
import qualified Data.Text as T
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)

import Types
import Contacts (Contacts)
import Contact (Contact)

import qualified File
import qualified Contacts
import qualified Contact

interpret :: Interpreter
interpret (Free (DisplayWelcomeBanner        x)) = UI.printWelcomeBanner >> interpret x
interpret (Free (DisplayMessage msg          x)) = UI.printMessage msg >> interpret x
interpret (Free (GetAction                   x)) = UI.getAction >>= interpret . x
interpret (Free (DisplayContactList contacts x)) = UI.listContacts contacts >> interpret x
interpret (Free (GetContact                  x)) = UI.getContact >>= interpret . x
interpret (Free (ReadContacts  path          x)) = File.readContacts path >>= interpret . x
interpret (Free (WriteContacts path contacts x)) = File.writeContacts path contacts >> interpret x
interpret (Free (Exit code)                    ) = exit code
interpret (Pure _)                               = exitSuccess

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


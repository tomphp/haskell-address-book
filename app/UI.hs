{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module UI where

import Control.Monad.Loops (untilJust)
import Data.Text (Text)
import System.Exit (ExitCode(ExitFailure), exitWith, exitSuccess)

import qualified Data.Text.IO as IO

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
    IO.putStrLn "=================================================================="
    IO.putStrLn "=== Welcome to the Haskell Phone Book                          ==="
    IO.putStrLn "=================================================================="

printMessage :: Text -> IO ()
printMessage msg = IO.putStrLn $ ">>> " <> msg

getAction :: IO Action
getAction =
    untilJust $ do
        printCommandList

        input <- IO.getLine

        case actionFromString input of
            Just action -> return $ Just action
            Nothing     -> printMessage "Bad command," >> return Nothing

actionFromString :: Text -> Maybe Action
actionFromString =
    \case
        "l" -> Just ListContacts
        "a" -> Just AddContact
        "s" -> Just Save
        "q" -> Just Quit
        _   -> Nothing

listContacts :: Contacts -> IO ()
listContacts contacts =
    Contacts.foreach contacts printContact

printContact :: Contact -> IO ()
printContact contact = do
    IO.putStrLn $ "Name:   " <> Contact.name contact
    IO.putStrLn $ "Number: " <> Contact.number contact
    IO.putStrLn "---"

getContact :: IO Contact
getContact =
    untilJust $ do
        IO.putStrLn "Enter Name:  "
        name <- IO.getLine

        IO.putStrLn "Enter Number:"
        number <- IO.getLine

        case Contact.new name number of
            Right contact -> return $ Just contact
            Left  msg     -> printMessage msg >> return Nothing

printCommandList :: IO ()
printCommandList = do
    IO.putStrLn "+-|Commands|-----------------------------------------------------+"
    IO.putStrLn "| l  List contacts                                               |"
    IO.putStrLn "| a  Add contact                                                 |"
    IO.putStrLn "| s  Save contacts                                               |"
    IO.putStrLn "| q  Quit                                                        |"
    IO.putStrLn "+----------------------------------------------------------------+"

exit :: Int -> IO ()
exit code = do
    IO.putStrLn "Exiting"
    case code of
         0 -> exitSuccess
         _ -> exitWith (ExitFailure code)


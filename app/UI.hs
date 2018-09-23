{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module UI (interpret) where

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
interpret (DisplayWelcomeBanner        x) = printWelcomeBanner >> x
interpret (DisplayMessage msg          x) = printMessage msg >> x
interpret (DisplayContactList contacts x) = listContacts contacts >> x
interpret (GetChoice msg               x) = getChoice msg >>= x
interpret (GetAction                   x) = getAction >>= x
interpret (GetContact                  x) = getContact >>= x
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
    untilRight $ do
        printCommandList

        textToAction <$> IO.getLine

textToAction :: Text -> Either Text Action
textToAction =
    \case
        "l" -> Right ListContacts
        "a" -> Right AddContact
        "s" -> Right Save
        "q" -> Right Quit
        _   -> Left "Bad command."

listContacts :: Contacts -> IO ()
listContacts contacts =
    Contacts.foreach contacts printContact

printContact :: Contact -> IO ()
printContact contact = do
    IO.putStrLn $ "Name:   " <> Contact.name contact
    IO.putStrLn $ "Number: " <> Contact.number contact
    IO.putStrLn "---"

getChoice :: Text -> IO Choice
getChoice msg = do
    IO.putStrLn $ msg <> " (y/n)"

    untilRight $ textToChoice <$> IO.getLine

textToChoice :: Text -> Either Text Choice
textToChoice =
    \case
        "y" -> Right Yes
        "n" -> Right No
        _   -> Left "Please enter y or n"

getContact :: IO Contact
getContact =
    untilRight $ do
        IO.putStrLn "Enter Name:  "
        name <- IO.getLine

        IO.putStrLn "Enter Number:"
        number <- IO.getLine

        return $ Contact.new name number

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

untilRight :: IO (Either Text a) -> IO a
untilRight action =
    untilJust $ do
        result <- action

        case result of
            Right x   -> return $ Just x
            Left  msg -> printMessage msg >> return Nothing
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

class Monad m => Console m where
    outputLine :: Text -> m ()
    readLine :: m Text
    exitProgram :: Int -> m ()

instance Console IO where
    outputLine = IO.putStrLn
    readLine = IO.getLine
    exitProgram code = case code of
                        0 -> exitSuccess
                        _ -> exitWith (ExitFailure code)

printWelcomeBanner :: Console m => m ()
printWelcomeBanner = do
    outputLine "=================================================================="
    outputLine "=== Welcome to the Haskell Phone Book                          ==="
    outputLine "=================================================================="

printMessage :: Console m => Text -> m ()
printMessage msg = outputLine $ ">>> " <> msg

getAction :: Console m => m Action
getAction =
    untilRight $ do
        printCommandList

        textToAction <$> readLine

textToAction :: Text -> Either Text Action
textToAction =
    \case
        "l" -> Right ListContacts
        "a" -> Right AddContact
        "s" -> Right Save
        "q" -> Right Quit
        _   -> Left "Bad command."

listContacts :: Console m => Contacts -> m ()
listContacts contacts =
    Contacts.foreach contacts printContact

printContact :: Console m => Contact -> m ()
printContact contact = do
    outputLine $ "Name:   " <> Contact.name contact
    outputLine $ "Number: " <> Contact.number contact
    outputLine "---"

getChoice :: Console m => Text -> m Choice
getChoice msg = do
    outputLine $ msg <> " (y/n)"

    untilRight $ textToChoice <$> readLine

textToChoice :: Text -> Either Text Choice
textToChoice =
    \case
        "y" -> Right Yes
        "n" -> Right No
        _   -> Left "Please enter y or n"

getContact :: Console m => m Contact
getContact =
    untilRight $ do
        outputLine "Enter Name:  "
        name <- readLine

        outputLine "Enter Number:"
        number <- readLine

        return $ Contact.new name number

printCommandList :: Console m => m ()
printCommandList = do
    outputLine "+-|Commands|-----------------------------------------------------+"
    outputLine "| l  List contacts                                               |"
    outputLine "| a  Add contact                                                 |"
    outputLine "| s  Save contacts                                               |"
    outputLine "| q  Quit                                                        |"
    outputLine "+----------------------------------------------------------------+"

exit :: Console m => Int -> m ()
exit code = outputLine "Exiting" >> exitProgram code

untilRight :: Console m => m (Either Text a) -> m a
untilRight action =
    untilJust $ do
        result <- action

        case result of
            Right x   -> return $ Just x
            Left  msg -> printMessage msg >> return Nothing
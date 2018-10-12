{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Integration.ConsoleUI (ConsoleUIT(..)) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))
import Control.Monad.Loops    (untilJust)
import Data.Text              (Text)
import System.Exit            (ExitCode(ExitFailure), exitWith, exitSuccess)

import qualified Data.Text.IO as IO

import Domain.Contact (Contact)

import qualified Domain.Action      as Action
import qualified Domain.Application as App
import qualified Domain.Choice      as Choice
import qualified Domain.Contact     as Contact
import qualified Domain.Contacts    as Contacts

newtype ConsoleUIT m a = ConsoleUIT { runConsoleUIT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , App.State
             , App.Config
             , App.Storage
             )

instance MonadTrans ConsoleUIT where
    lift = ConsoleUIT

class MonadIO m => Console m where
    outputLine  :: Text -> m ()
    readLine    :: m Text
    exitProgram :: Int -> m ()

instance MonadIO m => Console (ConsoleUIT m) where
    outputLine       = liftIO . IO.putStrLn
    readLine         = liftIO IO.getLine
    exitProgram code = case code of
                        0 -> liftIO exitSuccess
                        _ -> liftIO $ exitWith (ExitFailure code)

instance (Monad m, MonadIO m) => App.UI (ConsoleUIT m) where
    displayWelcomeBanner = do
        outputLine "=================================================================="
        outputLine "=== Welcome to the Haskell Phone Book                          ==="
        outputLine "=================================================================="

    displayMessage msg = outputLine $ ">>> " <> msg

    displayContactList contacts =
        Contacts.foreach contacts printContact

    getAction =
        untilRight $ do
            printCommandList
            textToAction <$> readLine

    getChoice msg = do
        outputLine $ msg <> " (y/n)"
        untilRight $ textToChoice <$> readLine

    getContact =
        untilRight $ do
            outputLine "Enter Name:  "
            name <- readLine

            outputLine "Enter Number:"
            number <- readLine

            return $ Contact.new name number

    exit code = outputLine "Exiting" >> exitProgram code

textToAction :: Text -> Either Text Action.Action
textToAction =
    \case
        "l" -> Right Action.ListContacts
        "a" -> Right Action.AddContact
        "s" -> Right Action.Save
        "q" -> Right Action.Quit
        _   -> Left "Bad command."


printContact :: Console m => Contact -> m ()
printContact contact = do
    outputLine $ "Name:   " <> Contact.name contact
    outputLine $ "Number: " <> Contact.number contact
    outputLine "---"


textToChoice :: Text -> Either Text Choice.Choice
textToChoice =
    \case
        "y" -> Right Choice.Yes
        "n" -> Right Choice.No
        _   -> Left "Please enter y or n"

printCommandList :: Console m => m ()
printCommandList = do
    outputLine "+-|Commands|-----------------------------------------------------+"
    outputLine "| l  List contacts                                               |"
    outputLine "| a  Add contact                                                 |"
    outputLine "| s  Save contacts                                               |"
    outputLine "| q  Quit                                                        |"
    outputLine "+----------------------------------------------------------------+"

untilRight :: (Console m, App.UI m) => m (Either Text a) -> m a
untilRight action =
    untilJust $ do
        result <- action

        case result of
            Right x   -> return $ Just x
            Left  msg -> App.displayMessage msg >> return Nothing

module Application
    ( Application
    , runApplication
    , getConfig
    , getAction
    , getContacts
    , putContacts
    , printWelcomeBanner
    , printMessage
    , listContacts
    , getContact
    , readContacts
    , writeContacts
    , exit
    ) where

import Control.Monad.Free (Free(..), iterM, liftF)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.State (StateT, runStateT, get, put)

import Data.Functor.Sum (Sum(..))

import qualified Data.Yaml as Yaml

import Contacts (Contacts)
import Contact (Contact)
import Types

type Program = Free (Sum UICommand StorageCommand)

type Application = StateT Contacts (ReaderT Config Program)

runApplication :: UIInterpreter () -> StorageInterpreter () -> Config -> Contacts -> Application () -> IO ()
runApplication ui storage config contacts application =
    interpret ui storage $ fst <$> runReaderT (runStateT application contacts) config

interpret :: UIInterpreter a -> StorageInterpreter a -> Program a -> IO a
interpret ui storage program =
  iterM go program
  where
    go (InL cmd) = ui cmd
    go (InR cmd) = storage cmd

getConfig :: Application Config
getConfig = ask

getContacts :: Application Contacts
getContacts = get

putContacts :: Contacts -> Application ()
putContacts = put

-- UI Freeness

printWelcomeBanner :: Application ()
printWelcomeBanner = uiOutputCommand DisplayWelcomeBanner

printMessage :: String -> Application ()
printMessage message = uiOutputCommand (DisplayMessage message)

getAction :: Application (Maybe Action)
getAction = uiInputCommand GetAction

listContacts :: Application ()
listContacts = getContacts >>= uiOutputCommand . DisplayContactList

getContact :: Application Contact
getContact = uiInputCommand GetContact

exit :: Int -> Application ()
exit code = liftUI (Exit code)

uiOutputCommand :: (() -> UICommand a) -> Application a
uiOutputCommand command = liftUI (command ())

uiInputCommand :: ((a -> a) -> UICommand b) -> Application b
uiInputCommand command = liftUI (command id)

liftUI :: UICommand a -> Application a
liftUI = liftFree . InL

-- Storage Freeness

readContacts :: Application (Either Yaml.ParseException Contacts)
readContacts = getConfig >>= storageInputCommand . ReadContacts . configFile

writeContacts :: Application ()
writeContacts = do
    config   <- getConfig
    contacts <- getContacts

    storageOutputCommand (WriteContacts (configFile config) contacts)

storageOutputCommand :: (() -> StorageCommand a) -> Application a
storageOutputCommand command = liftStorage (command ())

storageInputCommand :: ((a -> a) -> StorageCommand b) -> Application b
storageInputCommand command = liftStorage (command id)

liftStorage :: StorageCommand a -> Application a
liftStorage = liftFree . InR

liftFree :: Sum UICommand StorageCommand a -> Application
liftFree = lift . lift . liftF
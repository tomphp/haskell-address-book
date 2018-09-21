module Application where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT, runStateT, get, put)

import qualified Data.Yaml as Yaml

import Contacts (Contacts)
import Contact (Contact)
import Types

import qualified File
import qualified UI

type Application = StateT Contacts (ReaderT Config IO)

runApplication :: Config -> Contacts -> Application () -> IO ()
runApplication config contacts application =
    fst <$> runReaderT (runStateT application contacts) config

getConfig :: Application Config
getConfig = ask

getContacts :: Application Contacts
getContacts = get

putContacts :: Contacts -> Application ()
putContacts = put

-- UI Delegations

printWelcomeBanner :: Application ()
printWelcomeBanner = liftIO UI.printWelcomeBanner

getAction :: Application (Maybe Action)
getAction = liftIO UI.getAction

listContacts :: Application ()
listContacts = getContacts >>= liftIO . UI.listContacts

printContact :: Contact -> Application ()
printContact = liftIO . UI.printContact

getContact :: Application Contact
getContact = liftIO UI.getContact

printCommandList :: Application ()
printCommandList = liftIO UI.printCommandList

-- File

readContacts :: Application (Either Yaml.ParseException Contacts)
readContacts =
    Application.getConfig >>= liftIO . File.readContacts . configFile

writeContacts :: Application ()
writeContacts = do
    config  <- Application.getConfig
    contacts <- Application.getContacts

    liftIO $ File.writeContacts (configFile config) contacts
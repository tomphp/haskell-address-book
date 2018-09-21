module Application where

import Control.Monad.Free (Free(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.State (StateT, runStateT, get, put)

import qualified Data.Yaml as Yaml

import Contacts (Contacts)
import Contact (Contact)
import Types

type Application = StateT Contacts (ReaderT Config (Free Command))

runApplication :: Interpreter -> Config -> Contacts -> Application () -> IO ()
runApplication interpreter config contacts application =
    interpreter $ fst <$> runReaderT (runStateT application contacts) config

getConfig :: Application Config
getConfig = ask

getContacts :: Application Contacts
getContacts = get

putContacts :: Contacts -> Application ()
putContacts = put

-- Freeness

liftFree :: Free Command a -> Application a
liftFree = lift . lift

printWelcomeBanner :: Application ()
printWelcomeBanner = liftFree $ Free (DisplayWelcomeBanner (Pure ()))

printMessage :: String -> Application ()
printMessage message = liftFree $ Free (DisplayMessage message (Pure ()))

getAction :: Application (Maybe Action)
getAction = liftFree $ Free (GetAction Pure)

listContacts :: Application ()
listContacts = do
    contacts <- getContacts
    liftFree $ Free (DisplayContactList contacts (Pure ()))

getContact :: Application Contact
getContact = liftFree $ Free (GetContact Pure)

printCommandList :: Application ()
printCommandList = liftFree $ Free (DisplayCommandList (Pure ()))

readContacts :: Application (Either Yaml.ParseException Contacts)
readContacts = do
    config <- getConfig

    liftFree $ Free (ReadContacts (configFile config) Pure)

writeContacts :: Application ()
writeContacts = do
    config   <- getConfig
    contacts <- getContacts

    liftFree $ Free (WriteContacts (configFile config) contacts (Pure ()))

exit :: Int -> Application ()
exit code = liftFree $ Free (Exit code)

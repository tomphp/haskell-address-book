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

printWelcomeBanner :: Application ()
printWelcomeBanner = outputCommand DisplayWelcomeBanner

printMessage :: String -> Application ()
printMessage message = outputCommand (DisplayMessage message)

getAction :: Application (Maybe Action)
getAction = inputCommand GetAction

listContacts :: Application ()
listContacts = getContacts >>= outputCommand . DisplayContactList

getContact :: Application Contact
getContact = inputCommand GetContact

readContacts :: Application (Either Yaml.ParseException Contacts)
readContacts = getConfig >>= inputCommand . ReadContacts . configFile

writeContacts :: Application ()
writeContacts = do
    config   <- getConfig
    contacts <- getContacts

    outputCommand (WriteContacts (configFile config) contacts)

exit :: Int -> Application ()
exit code = liftFree $ Free (Exit code)

outputCommand :: (Free f () -> Command (Free Command a)) -> Application a
outputCommand command = liftFree $ Free (command (Pure ()))

inputCommand :: ((a -> Free f a) -> Command (Free Command b)) -> Application b
inputCommand command = liftFree $ Free (command Pure)

liftFree :: Free Command a -> Application a
liftFree = lift . lift
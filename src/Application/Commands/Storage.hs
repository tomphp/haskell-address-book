module Application.Commands.Storage where

import qualified Data.Functor.Sum as Sum
import qualified Data.Yaml as Yaml

import Application.Types.Base (Application, configFile)
import Application.Commands.Base (liftFree, getContacts, getConfig)
import Contacts (Contacts)

import qualified Application.Types.Storage as Storage

readContacts :: Application (Either Yaml.ParseException Contacts)
readContacts = getConfig >>= storageInputCommand . Storage.ReadContacts . configFile

writeContacts :: Application ()
writeContacts = do
    config   <- getConfig
    contacts <- getContacts

    storageOutputCommand (Storage.WriteContacts (configFile config) contacts)

storageOutputCommand :: (() -> Storage.Command a) -> Application a
storageOutputCommand command = liftStorage (command ())

storageInputCommand :: ((a -> a) -> Storage.Command b) -> Application b
storageInputCommand command = liftStorage (command id)

liftStorage :: Storage.Command a -> Application a
liftStorage = liftFree . Sum.InR


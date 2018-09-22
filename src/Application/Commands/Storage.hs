module Application.Commands.Storage where

import qualified Data.Functor.Sum as Sum
import qualified Data.Text as T

import Application.Types.Base (Application, configFile)
import Application.Commands.Base (liftFree, getContacts, getConfig, putContacts)

import qualified Application.Commands.UI as UI
import qualified Application.Types.Storage as Storage

readContacts :: Application ()
readContacts = do
    config   <- getConfig
    contacts <- storageInputCommand $ Storage.ReadContacts (configFile config)

    case contacts of
        Right cs  -> putContacts cs
        Left  err -> do UI.displayMessage (T.pack (show err))
                        UI.exit 1

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


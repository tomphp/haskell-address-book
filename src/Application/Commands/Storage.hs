module Application.Commands.Storage where

import qualified Data.Functor.Sum as Sum
import qualified Data.Text as T

import Application.Types.Base (Application)
import qualified Application.Commands.Base as App

import qualified Application.Commands.UI as UI
import qualified Application.Types.Storage as Storage

readContacts :: Application ()
readContacts = do
    filePath <- App.getFilePath
    contacts <- storageInputCommand $ Storage.ReadContacts filePath

    case contacts of
        Right cs  -> App.putContacts cs
        Left  err -> do UI.displayMessage (T.pack (show err))
                        UI.exit 1

writeContacts :: Application ()
writeContacts = do
    filePath <- App.getFilePath
    contacts <- App.getContacts

    storageOutputCommand (Storage.WriteContacts filePath contacts)

storageOutputCommand :: (() -> Storage.Command a) -> Application a
storageOutputCommand command = liftStorage (command ())

storageInputCommand :: ((a -> a) -> Storage.Command b) -> Application b
storageInputCommand command = liftStorage (command id)

liftStorage :: Storage.Command a -> Application a
liftStorage = App.liftFree . Sum.InR


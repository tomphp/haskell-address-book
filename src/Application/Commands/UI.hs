module Application.Commands.UI where

import Data.Text (Text)
import qualified Data.Functor.Sum as Sum

import Application.Types.Base (Application)
import Application.Commands.Base (liftFree, getContacts)
import Contact (Contact)

import qualified Application.Types.UI as UI

printWelcomeBanner :: Application ()
printWelcomeBanner = uiOutputCommand UI.DisplayWelcomeBanner

printMessage :: Text -> Application ()
printMessage message = uiOutputCommand (UI.DisplayMessage message)

getAction :: Application (Maybe UI.Action)
getAction = uiInputCommand UI.GetAction

listContacts :: Application ()
listContacts = getContacts >>= uiOutputCommand . UI.DisplayContactList

getContact :: Application Contact
getContact = uiInputCommand UI.GetContact

exit :: Int -> Application ()
exit code = liftUI (UI.Exit code)

uiOutputCommand :: (() -> UI.Command a) -> Application a
uiOutputCommand command = liftUI (command ())

uiInputCommand :: ((a -> a) -> UI.Command b) -> Application b
uiInputCommand command = liftUI (command id)

liftUI :: UI.Command a -> Application a
liftUI = liftFree . Sum.InL

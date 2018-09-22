module Application.Commands.UI
    ( displayWelcomeBanner
    , displayMessage
    , displayContactList
    , getAction
    , getContact
    , exit
    )
where

import Data.Text (Text)
import qualified Data.Functor.Sum as Sum

import Application.Types.Base (Application)
import Contact (Contact)

import qualified Application.Commands.Base as App
import qualified Application.Types.UI as UI

displayWelcomeBanner :: Application ()
displayWelcomeBanner = uiOutputCommand UI.DisplayWelcomeBanner

displayMessage :: Text -> Application ()
displayMessage message = uiOutputCommand (UI.DisplayMessage message)

displayContactList :: Application ()
displayContactList = App.getContacts >>= uiOutputCommand . UI.DisplayContactList

getAction :: Application UI.Action
getAction = uiInputCommand UI.GetAction

getContact :: Application Contact
getContact = uiInputCommand UI.GetContact

exit :: Int -> Application ()
exit code = liftUI (UI.Exit code)

uiOutputCommand :: (() -> UI.Command a) -> Application a
uiOutputCommand command = liftUI (command ())

uiInputCommand :: ((a -> a) -> UI.Command b) -> Application b
uiInputCommand command = liftUI (command id)

liftUI :: UI.Command a -> Application a
liftUI = App.liftFree . Sum.InL

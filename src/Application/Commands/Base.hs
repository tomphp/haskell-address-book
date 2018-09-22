module Application.Commands.Base where

import Control.Monad.Free (liftF, iterM)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State (runStateT, lift, put, get)
import Data.Functor.Sum (Sum(..))

import Application.Types.Base (Application, Program, Config)
import Contacts (Contacts)

import qualified Application.Types.Storage as Storage
import qualified Application.Types.UI as UI

runApplication :: UI.Interpreter () -> Storage.Interpreter () -> Config -> Contacts -> Application () -> IO ()
runApplication ui storage config contacts application =
    interpret ui storage $ fst <$> runReaderT (runStateT application contacts) config

interpret :: UI.Interpreter a -> Storage.Interpreter a -> Program a -> IO a
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

liftFree :: Sum UI.Command Storage.Command a -> Application a
liftFree = lift . lift . liftF

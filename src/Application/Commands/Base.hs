module Application.Commands.Base
    ( run
    , getFilePath
    , getContacts
    , putContacts
    , liftFree
    ) where

import Control.Monad.Free (liftF, iterM)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State (runStateT, lift, put, get)
import Data.Functor.Sum (Sum(..))

import Application.Types.Base (Application, Definition(..), Program, Config(configFile))
import Contacts (Contacts)

import qualified Application.Types.Storage as Storage
import qualified Application.Types.UI as UI
import qualified Contacts

run :: Definition () -> Application () -> IO ()
run Definition{userInterface = ui, storageSystem = storage, config = cfg} application =
    let interpret = interpreter ui storage
        reader    = runStateT application Contacts.new
        program   = fst <$> runReaderT reader cfg
    in interpret program

interpreter :: UI.Interpreter a -> Storage.Interpreter a -> Program a -> IO a
interpreter ui storage =
  iterM go
  where
    go (InL cmd) = ui cmd
    go (InR cmd) = storage cmd

getFilePath :: Application FilePath
getFilePath = configFile <$> getConfig

getConfig :: Application Config
getConfig = ask

getContacts :: Application Contacts
getContacts = get

putContacts :: Contacts -> Application ()
putContacts = put

liftFree :: Sum UI.Command Storage.Command a -> Application a
liftFree = lift . lift . liftF

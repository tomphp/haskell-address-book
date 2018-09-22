module Application.Types.Base where

import Control.Monad.Free (Free(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Functor.Sum (Sum(..))

import qualified Application.Types.Storage as Storage
import qualified Application.Types.UI as UI

import Contacts (Contacts)

data Config = Config { configFile :: FilePath }

type Program = Free (Sum UI.Command Storage.Command)

type Application = StateT Contacts (ReaderT Config Program)


{-# LANGUAGE RankNTypes #-}

module Application.Types.Base where

import Control.Monad.Free (Free(..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Data.Functor.Sum (Sum(..))

import Application.Types.State (State)

import qualified Application.Types.Storage as Storage
import qualified Application.Types.UI as UI

data Config = Config { configFile :: FilePath }

data Definition a = Definition
    { userInterface :: UI.Interpreter a
    , storageSystem :: Storage.Interpreter a
    , config        :: Config
    }

type Program = Free (Sum UI.Command Storage.Command)

type Application = StateT State (ReaderT Config Program)

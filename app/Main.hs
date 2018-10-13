{-# LANGUAGE OverloadedStrings #-}
module Main (main, mainMtl, mainFree) where

import System.Environment (getArgs)

import qualified Control.Monad.Reader     as R
import qualified Control.Monad.State      as ST
import qualified Control.Monad.Trans.Free as F

import Integration.ConsoleUI    as UI
-- import Integration.DummyStorage as Storage
import Integration.YamlStorage  as Storage

import qualified Application   as App
import qualified Domain.Config as Config
import qualified Domain.State  as State
import qualified Free.FreeApp as FreeApp

main :: IO ()
main = mainFree

-- MTL

mainMtl :: IO ()
mainMtl = do
    config <- loadConfig

    runReader config
        $ runState
        $ App.runApplicationT
        $ UI.runUIT
        $ Storage.runStorageT App.main

-- Free

mainFree :: IO ()
mainFree = loadConfig >>= runFree . buildFree

buildFree :: Config.Config -> F.Free FreeApp.Command ()
buildFree config = runReader config
                 $ runState
                 $ FreeApp.runFreeAppT
                 $ App.runApplicationT App.main

runFree :: F.Free FreeApp.Command () -> IO ()
runFree = UI.runUIT . Storage.runStorageT . interpreter
  where
    interpreter = F.iterM FreeApp.interpretCommand

---

runReader :: r -> R.ReaderT r m a -> m a
runReader = flip R.runReaderT

runState :: Functor m => ST.StateT State.State m a -> m a
runState state = fst <$> ST.runStateT state State.new

loadConfig :: IO Config.Config
loadConfig = do
    args <- getArgs

    return Config.Config { Config.configFile = head args }

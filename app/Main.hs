module Main (main, mainMtl, mainFree) where

import System.Environment (getArgs)

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as ST

import Integration.ConsoleUI   as UI
import Integration.YamlStorage as Storage

import qualified Application   as App
import qualified Domain.Config as Config
import qualified Domain.State  as State

main :: IO ()
main = mainFree

-- MTL

mainMtl :: IO ()
mainMtl = do
    config <- loadConfig
    fst <$> run config App.main
  where run config           = runReader config
                                . runState
                                . runApplication
                                . runUI
                                . runStorage
        runReader config app = R.runReaderT app config
        runState app         = ST.runStateT app State.new
        runApplication       = App.runApplicationT
        runUI                = UI.runConsoleUIT
        runStorage           = runYamlStorageT

-- Free

mainFree :: IO ()
mainFree = do
    config  <- loadConfig

    let appDef = App.Definition { App.config = config }

    runUI $ runStorage $ App.run appDef App.main
  where runUI                = UI.runConsoleUIT
        runStorage           = runYamlStorageT


loadConfig :: IO Config.Config
loadConfig = do
    args <- getArgs

    return Config.Config { Config.configFile = head args }
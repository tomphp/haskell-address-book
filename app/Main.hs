{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Application as App
import qualified UI
import qualified File

main :: IO ()
main = do
    config  <- loadConfig

    let appDef = App.Definition { App.userInterface = UI.interpret
                                , App.storageSystem = File.interpret
                                , App.config        = config
                                }

    App.run appDef App.main

loadConfig :: IO App.Config
loadConfig = do
    args <- getArgs

    return App.Config { App.configFile = head args }
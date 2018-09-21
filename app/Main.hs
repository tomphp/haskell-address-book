{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Contacts

import qualified ApplicationMain
import Application (runApplication)
import Types
import qualified UI

-- ApplicationMain

main :: IO ()
main = do
    putStrLn ""
    config  <- loadConfig
    runApplication UI.interpret config Contacts.new ApplicationMain.application

loadConfig :: IO Config
loadConfig = do
    args <- getArgs

    return Config { configFile = head args }
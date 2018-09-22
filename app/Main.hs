{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Contacts

import qualified ApplicationMain
import Application (runApplication)
import Types
import qualified UI
import qualified File

main :: IO ()
main = do
    config  <- loadConfig

    runApplication UI.interpret
                   File.interpret
                   config
                   Contacts.new
                   ApplicationMain.application

loadConfig :: IO Config
loadConfig = do
    args <- getArgs

    return Config { configFile = head args }
{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import qualified Contacts

import qualified Application
import qualified UI
import qualified File

main :: IO ()
main = do
    config  <- loadConfig

    Application.runApplication UI.interpret
                               File.interpret
                               config
                               Contacts.new
                               Application.main

loadConfig :: IO Application.Config
loadConfig = do
    args <- getArgs

    return Application.Config { Application.configFile = head args }
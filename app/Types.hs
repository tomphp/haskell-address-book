module Types where

data Action = ListContacts | AddContact | Quit

data Config = Config { configFile :: String }
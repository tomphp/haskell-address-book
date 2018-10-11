{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Integration.YamlStorage (YamlStorageT(..)) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))

import qualified Data.Text          as T
import qualified Data.Text.IO       as IO
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml          as Yaml

import Domain.Contacts (Contacts)

import qualified Domain.Application as App

class MonadIO m => FileAccess m where
    readFromFile :: FilePath -> m Text
    writeToFile  :: FilePath -> Text -> m ()

newtype YamlStorageT m a = YamlStorageT { runYamlStorageT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , App.Config
             , App.State
             , App.UI
             )

-- Why manual instances?
instance MonadTrans YamlStorageT where
    lift = YamlStorageT
-- End why

instance MonadIO m => FileAccess (YamlStorageT m) where
    readFromFile      = liftIO . IO.readFile
    writeToFile  path = liftIO . IO.writeFile path

instance (Functor m, MonadIO m) => App.Storage (YamlStorageT m) where
    readContacts = do
        contents <- readFromFile "addressbook.yaml"
        return $ case decodeContacts contents of
            Right contacts -> Right contacts
            Left  err      -> Left $ T.pack $ show err

    writeContacts =
        writeToFile "addressbook.yaml" . encodeContacts

decodeContacts :: Text -> Either Yaml.ParseException Contacts
decodeContacts = Yaml.decodeEither' . Encoding.encodeUtf8

encodeContacts :: Contacts -> Text
encodeContacts = Encoding.decodeUtf8 . Yaml.encode
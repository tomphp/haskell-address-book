{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Integration.YamlStorage (YamlStorageT(..)) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))
import Data.Text              (Text)
-- import GHC.Generics           (Generic)

import qualified Data.Text          as T
import qualified Data.Text.IO       as IO
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml          as Yaml

import qualified Domain.Application as App
-- import qualified Domain.Contact     as Contact
import qualified Domain.Contacts    as Contacts

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

instance MonadTrans YamlStorageT where
    lift = YamlStorageT

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

decodeContacts :: Text -> Either Yaml.ParseException Contacts.Contacts
decodeContacts = Yaml.decodeEither' . Encoding.encodeUtf8

encodeContacts :: Contacts.Contacts -> Text
encodeContacts = Encoding.decodeUtf8 . Yaml.encode

-- YamlContacts data type
--
-- data YamlContact = YamlContact { name :: Text
--                                , number :: Text
--                                } deriving (Eq, Generic, Show)
--
-- instance Yaml.FromJSON YamlContact
-- instance Yaml.ToJSON YamlContact
--
-- hydrateContact :: YamlContact -> Either Text Contact.Contact
-- hydrateContact YamlContact{name=name, number=number} = Contact.new name number
--
-- extractContact :: Contact.Contact -> YamlContact
-- extractContact contact = YamlContact { name   = Contact.name contact
--                                      , number = Contact.number contact}
--
-- -- YamlContact data type
--
-- newtype YamlContacts = YamlContacts [YamlContact] deriving (Generic)
--
-- instance Yaml.FromJSON YamlContacts
-- instance Yaml.ToJSON YamlContacts
--
-- hydrateContacts :: YamlContacts -> Contacts.Contacts
-- hydrateContacts = _
--
-- extractContacts :: Contacts.Contacts -> YamlContacts
-- extractContacts = _

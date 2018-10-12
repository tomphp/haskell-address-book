{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Integration.YamlStorage (YamlStorageT(..)) where

import Control.Monad          ((>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))
import Data.Text              (Text)
import GHC.Generics           (Generic)

import qualified Data.Bifunctor     as Bifunctor
import qualified Data.Text          as T
import qualified Data.Text.IO       as IO
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml          as Yaml

import qualified Domain.Application as App
import qualified Domain.Contact     as Contact
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
        return $ Bifunctor.first (T.pack . show) $ decodeContacts contents

    writeContacts =
        writeToFile "addressbook.yaml" . encodeContacts

decodeContacts :: Text -> Either Text Contacts.Contacts
decodeContacts =
    decodeText >=> hydrateContacts
  where
    decodeText  = errorToText . Yaml.decodeEither' . TE.encodeUtf8
    errorToText = Bifunctor.first (T.pack . show)


encodeContacts :: Contacts.Contacts -> Text
encodeContacts = TE.decodeUtf8 . Yaml.encode . extractContacts

-- YamlContacts data type

newtype YamlContacts = YamlContacts [YamlContact] deriving (Generic)

instance Yaml.FromJSON YamlContacts
instance Yaml.ToJSON YamlContacts

hydrateContacts :: YamlContacts -> Either Text Contacts.Contacts
hydrateContacts (YamlContacts yamlContacts) =
    buildContacts <$> traverse hydrateContact yamlContacts
  where
    buildContacts = foldl (flip Contacts.add) Contacts.new

extractContacts :: Contacts.Contacts -> YamlContacts
extractContacts = YamlContacts . map extractContact . Contacts.all

-- YamlContact data type

data YamlContact = YamlContact { name :: Text
                               , number :: Text
                               } deriving (Eq, Generic, Show)

instance Yaml.FromJSON YamlContact
instance Yaml.ToJSON YamlContact

hydrateContact :: YamlContact -> Either Text Contact.Contact
hydrateContact YamlContact{name=nam, number=num} = Contact.new nam num

extractContact :: Contact.Contact -> YamlContact
extractContact contact = YamlContact { name   = Contact.name contact
                                     , number = Contact.number contact}


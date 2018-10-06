module File (interpret) where

import Control.Monad ((>=>))
import Data.Text (Text)

import qualified Data.Text.IO as IO
import qualified Data.Text.Encoding as Encoding
import qualified Data.Yaml as Yaml

import Contacts (Contacts)
import Application.Types.Storage

interpret :: Interpreter a
interpret (ReadContacts  path          x) = readContacts path >>= x
interpret (WriteContacts path contacts x) = writeContacts path contacts >> x

class Monad m => FileAccess m where
    readFromFile :: FilePath -> m Text
    writeToFile :: FilePath -> Text -> m ()

instance FileAccess IO where
    readFromFile = IO.readFile
    writeToFile = IO.writeFile

readContacts :: FileAccess m => FilePath -> m (Either Yaml.ParseException Contacts)
readContacts =
    readFromFile >=> return . decodeContacts

writeContacts :: FileAccess m => FilePath -> Contacts -> m ()
writeContacts path =
    writeToFile path . encodeContacts

decodeContacts :: Text -> Either Yaml.ParseException Contacts
decodeContacts = Yaml.decodeEither' . Encoding.encodeUtf8

encodeContacts :: Contacts -> Text
encodeContacts = Encoding.decodeUtf8 . Yaml.encode
module File
    ( interpret
    ) where

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

readContacts :: FilePath -> IO (Either Yaml.ParseException Contacts)
readContacts =
    IO.readFile >=> return . decodeContacts

writeContacts :: FilePath -> Contacts -> IO ()
writeContacts path =
    IO.writeFile path . encodeContacts

decodeContacts :: Text -> Either Yaml.ParseException Contacts
decodeContacts = Yaml.decodeEither' . Encoding.encodeUtf8

encodeContacts :: Contacts -> Text
encodeContacts = Encoding.decodeUtf8 . Yaml.encode
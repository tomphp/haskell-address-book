module File
    ( interpret
    ) where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as C8
import qualified Data.Yaml as Yaml

import Contacts (Contacts)
import Application.Types.Storage

interpret :: Interpreter a
interpret (ReadContacts  path          x) = readContacts path >>= x
interpret (WriteContacts path contacts x) = writeContacts path contacts >> x

readContacts :: String -> IO (Either Yaml.ParseException Contacts)
readContacts =
    readFile >=> return . decodeContacts

writeContacts :: String -> Contacts -> IO ()
writeContacts path =
   writeFile path . encodeContacts

decodeContacts :: String -> Either Yaml.ParseException Contacts
decodeContacts = Yaml.decodeEither' . C8.pack

encodeContacts :: Contacts -> String
encodeContacts = C8.unpack . Yaml.encode
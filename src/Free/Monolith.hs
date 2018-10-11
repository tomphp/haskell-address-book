{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Free.Monolith (Definition(..), run) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))

import qualified Control.Monad.Trans.Free as F
import qualified Control.Monad.Reader     as R
import qualified Control.Monad.State      as ST
import qualified Data.Functor.Sum         as Sum
import qualified Data.Text                as T

import qualified Domain.Application as App
import qualified Domain.Config      as Config
import qualified Domain.State       as State

import Domain.Contact  (Contact)
import Domain.Contacts (Contacts)
import Domain.Action   (Action)
import Domain.Choice   (Choice)

data StorageCommand next = ReadContacts FilePath (Either T.Text Contacts -> next)
                         | WriteContacts FilePath Contacts next deriving (Functor)

data UICommand next = DisplayWelcomeBanner next
                  | DisplayMessage T.Text next
                  | DisplayContactList Contacts next
                  | GetChoice T.Text (Choice -> next)
                  | GetAction (Action -> next)
                  | GetContact (Contact -> next)
                  | Exit Int deriving (Functor)

data Definition a = Definition { config :: Config.Config }

type Command = (Sum.Sum UICommand StorageCommand)

type CommandFree = F.MonadFree Command

type FreeApplication = ST.StateT State.State (R.ReaderT Config.Config (F.Free Command))

run :: (MonadIO m, App.Storage m, App.UI m) => Definition () -> FreeApplication () -> m ()
run Definition{config = cfg} =
    interpreter . runReader . runState
  where
    runState state   = ST.runStateT state State.new
    runReader reader = fst <$> R.runReaderT reader cfg

interpreter :: (Monad m, MonadIO m, App.UI m, App.Storage m) => F.Free Command () -> m ()
interpreter = F.iterM interpretCommand

interpretCommand :: (Monad m, MonadIO m, App.UI m, App.Storage m) => Command (m ()) -> m ()
interpretCommand (Sum.InL cmd) = interpretUI cmd
interpretCommand (Sum.InR cmd) = interpretStorage cmd

interpretUI :: (App.UI m, MonadIO m) => UICommand (m ()) -> m ()
interpretUI (DisplayWelcomeBanner        x) = App.displayWelcomeBanner        >>  x
interpretUI (DisplayMessage msg          x) = App.displayMessage msg          >>  x
interpretUI (DisplayContactList contacts x) = App.displayContactList contacts >>  x
interpretUI (GetChoice msg               x) = App.getChoice msg               >>= x
interpretUI (GetAction                   x) = App.getAction                   >>= x
interpretUI (GetContact                  x) = App.getContact                  >>= x
interpretUI (Exit code)                     = App.exit code

interpretStorage :: App.Storage m => StorageCommand (m ()) -> m ()
interpretStorage (ReadContacts  _          x) = App.readContacts           >>= x
interpretStorage (WriteContacts _ contacts x) = App.writeContacts contacts >>  x

instance App.Config FreeApplication where
    getFilePath = Config.configFile <$> App.getConfig
    getConfig   = R.ask

instance App.State FreeApplication where
    getContacts          = State.getContacts <$> ST.get
    putContacts contacts = ST.modify (State.setContacts contacts)
    setUnsaved           = ST.modify State.setUnsaved
    setSaved             = ST.modify State.setSaved
    hasUnsaved           = State.hasUnsaved <$> ST.get

instance App.Storage FreeApplication where
    readContacts = do
        filePath <- App.getFilePath

        storageInputCommand $ ReadContacts filePath

    writeContacts contacts = do
        filePath <- App.getFilePath

        storageOutputCommand (WriteContacts filePath contacts)

storageOutputCommand :: CommandFree m => (() -> StorageCommand a) -> m a
storageOutputCommand command = liftStorage (command ())

storageInputCommand :: CommandFree m => ((a -> a) -> StorageCommand b) -> m b
storageInputCommand command = liftStorage (command id)

liftStorage :: CommandFree m => StorageCommand a -> m a
liftStorage = F.liftF . Sum.InR

instance App.UI FreeApplication where
    displayWelcomeBanner   = lift $ uiOutputCommand DisplayWelcomeBanner
    displayMessage message = lift $ uiOutputCommand (DisplayMessage message)
    displayContactList     = lift . uiOutputCommand . DisplayContactList
    getAction              = lift $ uiInputCommand GetAction
    getChoice msg          = lift $ uiInputCommand $ GetChoice msg
    getContact             = lift $ uiInputCommand GetContact
    exit code              = lift $ liftUI (Exit code)

uiOutputCommand :: CommandFree m => (() -> UICommand a) -> m a
uiOutputCommand command = liftUI (command ())

uiInputCommand :: CommandFree m => ((a -> a) -> UICommand b) -> m b
uiInputCommand command = liftUI (command id)

liftUI :: CommandFree m => UICommand a -> m a
liftUI = F.liftF . Sum.InL

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Free.Monolith (Command, FreeAppT(..), interpretCommand) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans    (MonadTrans(..), lift)

import qualified Control.Monad.Reader     as R
import qualified Control.Monad.State      as ST
import qualified Control.Monad.Trans.Free as F
import qualified Data.Functor.Sum         as Sum
import qualified Data.Text                as T

import qualified Domain.Application as App

import Domain.Action   (Action)
import Domain.Choice   (Choice)
import Domain.Contact  (Contact)
import Domain.Contacts (Contacts)

data StorageCommand next = ReadContacts (Either T.Text Contacts -> next)
                         | WriteContacts Contacts next deriving (Functor)

data UICommand next = DisplayWelcomeBanner next
                    | DisplayMessage T.Text next
                    | DisplayContactList Contacts next
                    | GetChoice T.Text (Choice -> next)
                    | GetAction (Action -> next)
                    | GetContact (Contact -> next)
                    | Exit Int deriving (Functor)

type Command = Sum.Sum UICommand StorageCommand

type CommandFree = F.MonadFree Command

newtype FreeAppT m a = FreeAppT { runFreeAppT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , App.Config
             , App.State
             )

mapFreeAppT :: (m a -> n b) -> FreeAppT m a -> FreeAppT n b
mapFreeAppT f = FreeAppT . f . runFreeAppT

instance MonadTrans FreeAppT where
    lift = FreeAppT

instance F.MonadFree f m => F.MonadFree f (FreeAppT m) where
    wrap = F.wrap

instance (Functor m, R.MonadReader c m) => R.MonadReader c (FreeAppT m) where
    ask   = lift R.ask
    local = mapFreeAppT . R.local

instance (Functor m, ST.MonadState c m) => ST.MonadState c (FreeAppT m) where
    get   = lift ST.get
    put   = lift . ST.put
    state = lift . ST.state

interpretCommand :: (Monad m, App.UI m, App.Storage m) => Command (m ()) -> m ()
interpretCommand (Sum.InL cmd) = interpretUI cmd
interpretCommand (Sum.InR cmd) = interpretStorage cmd

interpretUI :: App.UI m => UICommand (m ()) -> m ()
interpretUI (DisplayWelcomeBanner        x) = App.displayWelcomeBanner        >>  x
interpretUI (DisplayMessage msg          x) = App.displayMessage msg          >>  x
interpretUI (DisplayContactList contacts x) = App.displayContactList contacts >>  x
interpretUI (GetChoice msg               x) = App.getChoice msg               >>= x
interpretUI (GetAction                   x) = App.getAction                   >>= x
interpretUI (GetContact                  x) = App.getContact                  >>= x
interpretUI (Exit code)                     = App.exit code

interpretStorage :: App.Storage m => StorageCommand (m ()) -> m ()
interpretStorage (ReadContacts           x) = App.readContacts           >>= x
interpretStorage (WriteContacts contacts x) = App.writeContacts contacts >>  x

instance (CommandFree m) => App.Storage (FreeAppT m) where
    readContacts  = storageInputCommand ReadContacts
    writeContacts = storageOutputCommand . WriteContacts

storageOutputCommand :: CommandFree m => (() -> StorageCommand a) -> FreeAppT m a
storageOutputCommand command = liftStorage (command ())

storageInputCommand :: CommandFree m => ((a -> a) -> StorageCommand b) -> FreeAppT m b
storageInputCommand command = liftStorage (command id)

liftStorage :: CommandFree m => StorageCommand a -> FreeAppT m a
liftStorage = FreeAppT . F.liftF . Sum.InR

instance CommandFree m => App.UI (FreeAppT m) where
    displayWelcomeBanner   = uiOutputCommand DisplayWelcomeBanner
    displayMessage message = uiOutputCommand (DisplayMessage message)
    displayContactList     = uiOutputCommand . DisplayContactList
    getAction              = uiInputCommand GetAction
    getChoice msg          = uiInputCommand $ GetChoice msg
    getContact             = uiInputCommand GetContact
    exit code              = liftUI (Exit code)

uiOutputCommand :: CommandFree m => (() -> UICommand a) -> FreeAppT m a
uiOutputCommand command = liftUI (command ())

uiInputCommand :: CommandFree m => ((a -> a) -> UICommand b) -> FreeAppT m b
uiInputCommand command = liftUI (command id)

liftUI :: CommandFree m => UICommand a -> FreeAppT m a
liftUI = FreeAppT . F.liftF . Sum.InL

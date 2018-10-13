{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

module Free.FreeApp (Command, FreeAppT(..), interpretCommand) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans    (MonadTrans(..), lift)

import qualified Control.Monad.Reader     as R
import qualified Control.Monad.State      as ST
import qualified Control.Monad.Trans.Free as F
import qualified Data.Functor.Sum         as Sum
import qualified Data.Text                as T

import Domain.Action   (Action)
import Domain.Choice   (Choice)
import Domain.Contact  (Contact)
import Domain.Contacts (Contacts)

import qualified Domain.Application as App

-- FreeAppT

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

-- Interpreter

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

-- Commands

type Command = Sum.Sum UICommand StorageCommand

data StorageCommand next = ReadContacts (Either T.Text Contacts -> next)
                         | WriteContacts Contacts next deriving (Functor)

data UICommand next = DisplayWelcomeBanner next
                    | DisplayMessage T.Text next
                    | DisplayContactList Contacts next
                    | GetChoice T.Text (Choice -> next)
                    | GetAction (Action -> next)
                    | GetContact (Contact -> next)
                    | Exit Int deriving (Functor)

class Functor f => SubCommand f where
    wrap :: f a -> Command a

instance SubCommand UICommand where
    wrap = Sum.InL

instance SubCommand StorageCommand where
    wrap = Sum.InR

-- Implementations

type CommandFree = F.MonadFree Command

instance (CommandFree m) => App.Storage (FreeAppT m) where
    readContacts  = inputCommand ReadContacts
    writeContacts = outputCommand . WriteContacts

instance CommandFree m => App.UI (FreeAppT m) where
    displayWelcomeBanner = outputCommand DisplayWelcomeBanner
    displayMessage msg   = outputCommand $DisplayMessage msg
    displayContactList   = outputCommand . DisplayContactList
    getAction            = inputCommand GetAction
    getChoice msg        = inputCommand $ GetChoice msg
    getContact           = inputCommand GetContact
    exit code            = liftCommand (Exit code)

outputCommand :: (SubCommand cmd, CommandFree m) => (() -> cmd a) -> FreeAppT m a
outputCommand command = liftCommand (command ())

inputCommand :: (SubCommand cmd, CommandFree m) => ((a -> a) -> cmd b) -> FreeAppT m b
inputCommand command = liftCommand (command id)

liftCommand :: (SubCommand cmd, CommandFree m) => cmd a -> FreeAppT m a
liftCommand = FreeAppT . F.liftF . wrap

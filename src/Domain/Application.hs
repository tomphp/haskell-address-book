{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module Domain.Application where

import           Control.Monad.IO.Class
import qualified Control.Monad.Reader   as R
import qualified Control.Monad.State    as ST
import qualified Control.Monad.Trans    as Trans
import qualified Data.Text              as T

import qualified Domain.Action   as Action
import qualified Domain.Choice   as Choice
import qualified Domain.Config   as Config
import qualified Domain.Contact  as Contact
import qualified Domain.Contacts as Contacts
import qualified Domain.State    as State

class Monad m => Config m where
    getFilePath :: m FilePath
    getConfig   :: m Config.Config

class Monad m => State m where
    getContacts :: m Contacts.Contacts
    putContacts :: Contacts.Contacts -> m ()
    setUnsaved  :: m ()
    setSaved    :: m ()
    hasUnsaved  :: m Bool

class Monad m => UI m where
    displayWelcomeBanner :: m ()
    displayMessage       :: T.Text -> m ()
    displayContactList   :: Contacts.Contacts -> m ()
    getAction            :: m Action.Action
    getChoice            :: T.Text -> m Choice.Choice
    getContact           :: m Contact.Contact
    exit                 :: Int -> m ()

class Monad m => Storage m where
    readContacts  :: m (Either T.Text Contacts.Contacts)
    writeContacts :: Contacts.Contacts -> m ()

newtype ApplicationT m a = ApplicationT { runApplicationT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , Storage
             , UI
             )

mapApplicationT :: (m a -> n b) -> ApplicationT m a -> ApplicationT n b
mapApplicationT f m = ApplicationT $ f (runApplicationT m)

instance Trans.MonadTrans ApplicationT where
    lift = ApplicationT

instance (Functor m, R.MonadReader c m) => R.MonadReader c (ApplicationT m) where
    ask   = Trans.lift R.ask
    local = mapApplicationT . R.local

instance (Functor m, ST.MonadState c m) => ST.MonadState c (ApplicationT m) where
    get   = Trans.lift ST.get
    put   = Trans.lift . ST.put
    state = Trans.lift . ST.state

instance R.MonadReader Config.Config m => Config (ApplicationT m) where
    getFilePath = Config.configFile <$> getConfig
    getConfig   = R.ask

instance ST.MonadState State.State m => State (ApplicationT m) where
    getContacts          = State.getContacts <$> ST.get
    putContacts contacts = ST.modify (State.setContacts contacts)
    setUnsaved           = ST.modify State.setUnsaved
    setSaved             = ST.modify State.setSaved
    hasUnsaved           = State.hasUnsaved <$> ST.get

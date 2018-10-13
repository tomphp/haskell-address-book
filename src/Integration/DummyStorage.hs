{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Integration.DummyStorage (DummyStorageT(..)) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans    (MonadTrans(..))

import qualified Data.Text.IO       as TIO

import qualified Domain.Application as App
import qualified Domain.Contacts    as Contacts

newtype DummyStorageT m a = DummyStorageT { runStorageT :: m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , App.Config
             , App.State
             , App.UI
             )

instance MonadTrans DummyStorageT where
    lift = DummyStorageT

instance (Functor m, MonadIO m) => App.Storage (DummyStorageT m) where
    readContacts = do liftIO $ TIO.putStrLn "Reading :: Returning empty contacts"
                      return (Right Contacts.new)

    writeContacts _ = liftIO $ TIO.putStrLn "Saving to disk"


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module StackTest where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

newtype App s r m a = App { runApp :: StateT s (ReaderT r m) a }
    deriving (Functor
            , Applicative
            , Monad
            -- , MonadTrans r
            , MonadReader r
            )

class One m where
    getOne :: m String

instance One App where
    getOne = ask


main :: IO ()
main = do
    let state = runApp run
    let reader = runStateT state "Hello"
    runReaderT reader "Sir"
    putStrLn "Done"

run :: App String String IO ()
run = App action

action :: (MonadReader String m, MonadState String m, MonadIO m) => m ()
action = do
    salutation <- ask
    title <- get

    liftIO $ putStrLn $ salutation ++ title
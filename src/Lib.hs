{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    (
    ) where

import qualified System.Process.Typed as SPT
import qualified System.Process as SP
import System.FilePath.Glob (Pattern, globDir1)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad (void, (>=>))
import System.IO (withBinaryFile, IOMode(..), Handle, stdin, stdout, stderr, hClose, openFile, hFlush, hPutStrLn)
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Utf
import Data.String
import Control.Monad.State (StateT, get, put, state, runStateT)
import Data.Functor.Identity (Identity)
import Data.Text (Text, unpack, pack)

data Handles = Handles Handle Handle Handle

newtype GushT m a =
  GushT { unGushT :: StateT Handles (ResourceT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

type Gush a = GushT IO a

type PipelineT m a b = a -> GushT m b
type Pipeline a b = a -> Gush b

class Monad m => MonadGush m where
  getHandles :: m Handles
  putHandles :: Handles -> m ()
  stateHandles :: (Handles -> (a, Handles)) -> m a

instance Monad m => MonadGush (GushT m) where
  getHandles = GushT get
  putHandles = GushT <$> put
  stateHandles = GushT <$> state

runGushT :: MonadUnliftIO m => GushT m a -> m (a, Handles)
runGushT (GushT s) = runResourceT $ runStateT s (Handles stdin stdout stderr)

runGush :: Gush a -> IO (a, Handles)
runGush = runGushT

exec :: (() -> Gush a) -> IO a
exec f = fmap fst $ runGush $ f ()

cmd :: (MonadGush m, MonadResource m) => Text -> [Text] -> (() -> m ())
cmd name args = \() -> do
  Handles hin hout herr <- getHandles
  void $ allocate
    (SPT.startProcess
      (SPT.setStdin (SPT.useHandleOpen hin) $
       SPT.setStdout (SPT.useHandleOpen hout) $
       SPT.setStderr (SPT.useHandleOpen herr) $
       SPT.proc (unpack name) (unpack <$> args)))
    (void . SPT.waitExitCode)

(|>) :: (MonadIO m, MonadGush m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f |> g = \x -> do
  Handles hin hout herr <- getHandles
  (pipeOut, pipeIn) <- liftIO SP.createPipe
  putHandles $ Handles hin pipeIn herr
  y <- f x
  liftIO $ hClose pipeIn
  putHandles $ Handles pipeOut hout herr
  z <- g y
  liftIO $ hClose pipeOut
  putHandles $ Handles hin hout herr
  return z

captureIn :: (MonadGush m, MonadResource m) => FilePath -> (a -> m a)
captureIn fp = \x -> do
  Handles hin hout herr <- getHandles
  (_, hout') <- allocate (openFile fp WriteMode) hClose
  putHandles $ Handles hin hout' herr
  return x

(|>>) :: (MonadGush m, MonadResource m) => (a -> m b) -> FilePath -> (a -> m b)
p |>> fp = captureIn fp >=> p

ls :: (MonadGush m, MonadResource m) => () -> m ()
ls = cmd "ls" ["--color=auto"]

cat :: (MonadGush m, MonadResource m) => () -> m ()
cat = cmd "cat" []

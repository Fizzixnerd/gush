{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    (
    ) where

import Prelude hiding (return, (>>=), (>>))
import qualified Prelude
import qualified System.Process.Typed as SPT
import qualified System.Process as SP
import System.FilePath.Glob (Pattern, globDir1)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad (void, (>=>))
import System.IO (withBinaryFile, IOMode(..), Handle, stdin, stdout, stderr, hClose, openFile, hFlush, hPutStrLn)
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Utf
import Data.Maybe (fromMaybe)
import Data.String
import Control.Monad.State (MonadState, StateT, get, put, state, modify, runStateT)
import Data.Functor.Identity (Identity)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.IO as TIO
import qualified System.Environment as Env
import TH

data Handles = Handles Handle Handle Handle

newtype GushT m a =
  GushT { unGushT :: StateT Handles (ResourceT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadResource)

type Gush a = GushT IO a

class MonadResource m => MonadGush m where
  getHandles :: m Handles
  putHandles :: Handles -> m ()
  stateHandles :: (Handles -> (a, Handles)) -> m a

instance MonadIO m => MonadGush (GushT m) where
  getHandles = GushT get
  putHandles = GushT <$> put
  stateHandles = GushT <$> state

instance MonadTrans GushT where
  lift = GushT . lift . lift

modifyHandles :: MonadGush m => (Handles -> Handles) -> m ()
modifyHandles f = putHandles =<< f <$> getHandles

class Return a b where
  return :: a -> b

instance (Monad m, a ~ a') => Return a (m a') where
  return = Prelude.return

class Bind a b c where
  (>>=) :: a -> (b -> c) -> c

instance (Monad m, Monad m', a ~ a', m ~ m') => Bind (m a) a' (m' b) where
  (>>=) = (Prelude.>>=)

class Then a b where
  (>>) :: a -> b -> b

instance (Monad m, Monad m', m ~ m') => Then (m a) (m' b) where
  (>>) = (Prelude.>>)

instance {-# OVERLAPPING #-} Then (Pipeline (GushT IO) () a) (Pipeline (GushT IO) () b)  where
  f >> g = \() -> (liftIO $ exec f) Prelude.>> (liftIO $ exec g)

-- | Pipeline Gush () a -> IO a
exec :: (() -> Gush a) -> IO a
exec f = fmap fst $ runGush $ f ()

type Pipeline m a b = a -> m b

-- end :: Monoid w => w
-- end = mempty

-- (>>) = (<>)

runGushT :: MonadUnliftIO m => GushT m a -> m (a, Handles)
runGushT (GushT s) = runResourceT $ runStateT s (Handles stdin stdout stderr)

runGush :: Gush a -> IO (a, Handles)
runGush = runGushT

f :: Monad m => a -> m a
f x = do
  y <- f x
  return y

cmd :: MonadGush m => Text -> [Text] -> Pipeline m () ()
cmd name args = \() -> do
  Handles hin hout herr <- getHandles
  void $ allocate
    (SPT.startProcess
      (SPT.setStdin (SPT.useHandleOpen hin) $
       SPT.setStdout (SPT.useHandleOpen hout) $
       SPT.setStderr (SPT.useHandleOpen herr) $
       SPT.proc (unpack name) (unpack <$> args)))
    (void . SPT.waitExitCode)

-- | Has no left identity
(|.) :: MonadGush m => Pipeline m a b -> Pipeline m b c -> Pipeline m a c
f |. g = \x -> do
  Handles hin hout herr <- getHandles
  (_, (pipeOut, pipeIn)) <- allocate SP.createPipe $ \(pout, pin) -> do
    hClose pin
    hClose pout
  putHandles $ Handles hin pipeIn herr
  y <- f x
  liftIO $ hClose pipeIn
  putHandles $ Handles pipeOut hout herr
  z <- g y
  liftIO $ hClose pipeOut
  putHandles $ Handles hin hout herr
  return z

(|$) :: Monad m => Pipeline m a b -> m a -> m b
(|$) = (=<<)

captureAs :: MonadGush m => IOMode -> FilePath -> Pipeline m a a
captureAs mode fp = \x -> do
  (_, hout') <- allocate (openFile fp mode) hClose
  modifyHandles (\(Handles hin _ herr) -> Handles hin hout' herr)
  return x

captureIn :: MonadGush m => FilePath -> Pipeline m a a
captureIn = captureAs WriteMode

appendIn :: MonadGush m => FilePath -> Pipeline m a a
appendIn = captureAs AppendMode

(|>) :: MonadGush m => Pipeline m a b -> FilePath -> Pipeline m a b
f |> fp = captureIn fp >=> f

(|>>) :: MonadGush m => Pipeline m a b-> FilePath -> Pipeline m a b
f |>> fp = appendIn fp >=> f

readFrom :: MonadGush m => FilePath -> Pipeline m a a
readFrom fp = \x -> do
  (_, hin') <- allocate (openFile fp ReadMode) hClose
  modifyHandles (\(Handles _ hout herr) -> Handles hin' hout herr)
  return x

here :: MonadGush m => Text -> Pipeline m a a
here t = \x -> do
  (_, (pipeOut, pipeIn)) <- allocate SP.createPipe $ \(pout, pin) -> do
    hClose pin
    hClose pout
  liftIO $ TIO.hPutStr pipeIn t
  liftIO $ hClose pipeIn
  modifyHandles (\(Handles _ hout herr) -> Handles pipeOut hout herr)
  return x

(<|) :: MonadGush m => Pipeline m a b -> FilePath -> Pipeline m a b
f <| fp = readFrom fp >=> f

(<<|) :: MonadGush m => Pipeline m a b -> Text -> Pipeline m a b
f <<| t = here t >=> f

getEnv :: MonadIO m => Text -> m (Maybe ByteString)
getEnv = fmap (fmap Utf.pack) . liftIO . Env.lookupEnv . unpack

getEnv' :: MonadIO m => Text -> m ByteString
getEnv' = fmap (fromMaybe BS.empty) . getEnv

setEnv :: MonadIO m => Text -> ByteString -> m ()
setEnv name value = liftIO $ Env.setEnv (unpack name) (Utf.unpack value)

unsetEnv :: MonadIO m => Text -> m ()
unsetEnv = liftIO . Env.unsetEnv . unpack

ls :: MonadGush m => () -> m ()
ls = let ls' = "ls" in cmd [text|#{ls'}|] ["--color=auto"]

cat :: MonadGush m => () -> m ()
cat = cmd "cat" []

cmd' :: Text -> [Text] -> () -> Gush ()
cmd' = cmd

testPipe :: IO ()
testPipe = exec $ do
  cmd' "echo" [[text|rawr|]] |. cat |> "myfile.txt"
  let x = "leaf!" in cat <<| [text|hello #{x}|]

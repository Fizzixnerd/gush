{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
    (
    ) where

import Prelude hiding ((.), id)
import qualified System.Process.Typed as SPT
import qualified System.Process as SP
import System.FilePath.Glob (Pattern, globDir1)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad (void)
import System.IO (withBinaryFile, IOMode(..), Handle, stdin, stdout, stderr, hClose, openFile, hFlush, hPutStrLn)
import Control.Category
import Control.Arrow
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Directory
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Utf
import Data.String

data Handles = Handles Handle Handle Handle

-- newtype GushT m a =
--   GushT { unGushT :: StateT Handles  }

newtype Pipeline m i o =
  Pipeline ((i, Handles) -> m (o, Handles))

instance Monad m => Category (Pipeline m) where
  id = Pipeline return
  (Pipeline f) . (Pipeline g) = Pipeline $ \(x, handles) -> do
    (y, handles') <- g (x, handles)
    f (y, handles')

instance Monad m => Arrow (Pipeline m) where
  arr f = Pipeline $ \(x, handles) -> return (f x, handles)
  first (Pipeline f) = Pipeline $ \((x, y), handles) -> do
    (z, handles'') <- f (x, handles)
    return ((z, y), handles'')

instance (Monad m, IsString s) => IsString (Pipeline m () s) where
  fromString s = arr (const $ fromString s)

liftP :: Monad m => (a -> m b) -> Pipeline m a b
liftP f = Pipeline $ \(x, handles) -> do
  x <- f x
  return (x, handles)

exec' :: MonadIO m => Pipeline m () o -> m o
exec' (Pipeline f) = fst <$> f ((), Handles stdin stdout stderr)

exec :: MonadUnliftIO m => Pipeline (ResourceT m) () a -> m a
exec = runResourceT . exec'

-- | The flip of this cannot be made the definition of (.) because
-- it would violate the identity law.
(|>) :: MonadResource m => Pipeline m a b -> Pipeline m b c -> Pipeline m a c
f |> g = proc x -> do
  Handles hin hout herr <- getHandles -< ()
  (pipeOut, pipeIn) <- liftP $ liftIO . const SP.createPipe -< ()
  () <- setHandles -< Handles hin pipeIn herr
  y <- f -< x
  () <- liftP $ liftIO . hClose -< pipeIn
  () <- setHandles -< Handles pipeOut hout herr
  z <- g -< y
  () <- liftP $ liftIO . hClose -< pipeOut
  () <- setHandles -< Handles hin hout herr
  returnA -< z

cmd :: (MonadUnliftIO m, MonadResource m) => String -> [String] -> Pipeline m () ()
cmd name args =
  Pipeline $ \((), Handles hin hout herr) -> do
    (_, process) <-
      allocate
        (SPT.startProcess
          (SPT.setStdin (SPT.useHandleOpen hin) $
           SPT.setStdout (SPT.useHandleOpen hout) $
           SPT.setStderr (SPT.useHandleOpen herr) $
           SPT.proc name args))
        (void . SPT.waitExitCode)
    return ((), Handles hin hout herr)

-- cmd :: (MonadUnliftIO m, MonadResource m) => String -> [String] -> Pipeline m () ()
-- cmd name args =
--   Pipeline $ \((), Handles hin hout herr) -> do
--     SPT.withProcessWait
--       (SPT.setStdin (SPT.useHandleOpen hin) $
--        SPT.setStdout (SPT.useHandleOpen hout) $
--        SPT.setStderr (SPT.useHandleOpen herr) $
--        SPT.proc name args)
--       (const $ return ((), Handles hin hout herr))

-- cmd :: (MonadUnliftIO m, MonadResource m) => String -> [String] -> Pipeline m () ()
-- cmd name args =
--   Pipeline $ \((), Handles hin hout herr) -> do
--     void $
--       SPT.startProcess $
--       SPT.setStdin (SPT.useHandleOpen hin) $
--       SPT.setStdout (SPT.useHandleOpen hout) $
--       SPT.setStderr (SPT.useHandleOpen herr) $
--       SPT.proc name args
--     return ((), Handles hin hout herr)

captureIn :: MonadResource m => FilePath -> Pipeline m a a
captureIn fp =
  Pipeline $ \(x, Handles hin hout herr) -> do
    (_, hout') <- allocate (openFile fp WriteMode) hClose
    return (x, Handles hin hout' herr)

(|>>) :: MonadResource m => Pipeline m i o -> FilePath -> Pipeline m i o
p |>> fp = captureIn fp >>> p

getHandles :: Monad m => Pipeline m i Handles
getHandles = Pipeline $ \(_, handles) -> return (handles, handles)

setHandles :: Monad m => Pipeline m Handles ()
setHandles = Pipeline $ \(handles, _) -> return ((), handles)

ls :: (MonadUnliftIO m, MonadResource m) => Pipeline m () ()
ls = cmd "ls" ["--color=auto"]

echo :: MonadIO m => Pipeline m ByteString ()
echo = proc bs -> do
  Handles _ hout _ <- getHandles -< ()
  liftP $ liftIO . uncurry BS.hPutStr -< (hout, bs)

ls' :: MonadIO m => Pipeline m FilePath [FilePath]
ls' = proc fp -> do
  contents <- liftP $ liftIO . listDirectory -< fp
  echo -< (BS.intercalate "\n" $ Utf.pack <$> contents) <> "\n"
  returnA -< contents

testPipe2 :: (MonadUnliftIO m, MonadResource m) => Pipeline m () [FilePath]
testPipe2 = "." |> ls'

testPipe :: (MonadUnliftIO m, MonadResource m) => Pipeline m () ()
testPipe = ls |> cmd "cat" [] |> cmd "cat" []

{-
  Law checking:
  arr id = Pipeline $ (\x, handles) -> return (id x, handles)
         = Pipeline return
         = id

  arr (f >>> g) = Pipeline $ \(x, handles) -> return ((f >>> g) x, handles)
  arr f >>> arr g = (Pipeline $ \(x, hanldes) -> return (g x, handles)) . (Pipeline $ \(x, handles) -> return (f x, handles))
                  = g' . f'
                  = Pipeline $ \(x, handles) -> do
                      (y, handles') <- f' (x, handles)
                      g' (y, handles')
                  = Pipeline $ \(x, handles) -> do
                      (y, handles') <- return (f x, handles)
                      g' (y, handles')
                  = Pipeline $ \(x, handles) -> do
                      return ((f >>> g) x, handles)

  first (arr f) = Pipeline $ \((x, y), handles) -> do
    (z, handles') <- return (f x, handles)
    return ((z, y), handles')
                = Pipeline $ \((x, y), handles) -> do
    return ((f x, y), handles)
                = arr (first f)

-}

-- (|>) :: MonadUnliftIO m => Segment m () -> Segment m r -> Segment m r
-- (|>) = ($|)

-- (|?>) :: MonadUnliftIO m => Segment m () -> Segment m r -> Segment m r


-- forEachFile_ :: MonadIO m => Pattern -> (FilePath -> Segment m r) -> Segment m ()
-- forEachFile_ pat f = do
--   fps <- liftIO $ globDir1 pat "."
--   mapM_ f fps

-- forEachFile :: MonadIO m => Pattern -> (FilePath -> Segment m r) -> Segment m [r]
-- forEachFile pat f = do
--   fps <- liftIO $ globDir1 pat "."
--   mapM f fps

-- ls :: ProcessType r => r
-- ls = PATH.ls "--color=auto"

-- emacs :: ProcessType r => r
-- emacs = PATH.emacsclient "--alternate-editor=\"\"" "--create-frame"

-- tmacs :: ProcessType r => r
-- tmacs = PATH.emacsclient "--alternate-editor=\"\"" "--tty"

-- someFunc :: IO ()
-- someFunc = runResourceT $ run $ do
--   ls
--   ts <- texts $ forEachFile_ "src/**" (grep "-e" "grepadoodledo") <|> (return ())
--   ls |> (conduit $ sinkFile "ls.txt")
--   liftIO $ putStrLn $ unwords $ unpack <$> ts

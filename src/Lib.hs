{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

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
import System.IO (withBinaryFile, IOMode(..), Handle, stdin, stdout, stderr, hClose, openFile, hFlush)
import Control.Category
import Control.Arrow

data Handles = Handles Handle Handle Handle

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
    (z, handles') <- f (x, handles)
    return ((z, y), handles')

exec :: Functor m => Pipeline m () o -> m o
exec (Pipeline f) = fst <$> f ((), Handles stdin stdout stderr)

execRes :: MonadUnliftIO m => Pipeline (ResourceT m) () a -> m a
execRes = runResourceT . exec

exec' :: Functor m => Pipeline m i o -> i -> m o
exec' (Pipeline f) x = fst <$> f (x, Handles stdin stdout stderr)

execWith :: Functor m => Pipeline m i o -> i -> Handles -> m o
execWith (Pipeline f) x handles = fst <$> f (x, handles)

-- | The flip of this cannot be made the definition of (.) because
-- it would violate the identity law.  Also, the pipes are not closed here;
-- include resourcet and close them!
(|>) :: MonadResource m => Pipeline m a b -> Pipeline m b c -> Pipeline m a c
(Pipeline f) |> (Pipeline g) = Pipeline $ \(x, Handles in1 out1 err1) -> do
  (releaseKey, (pipeOut, pipeIn)) <- allocate SP.createPipe $ \(pin, pout) -> do
    hClose pin
    hClose pout
  (y, Handles in2 out2 err2) <- f (x, Handles in1 pipeIn err1)
  (z, Handles in3 out3 err3) <- g (y, Handles pipeOut out1 err1)
  release releaseKey
  return (z, Handles in1 out1 err1)

-- cmd :: (MonadUnliftIO m, MonadResource m) => String -> [String] -> Pipeline m () ()
-- cmd name args =
--   Pipeline $ \((), Handles hin hout herr) -> do
--     (releaseKey, process) <-
--       allocate
--         (SPT.startProcess
--           (SPT.setStdin (SPT.useHandleOpen hin) $
--            SPT.setStdout (SPT.useHandleOpen hout) $
--            SPT.setStderr (SPT.useHandleOpen herr) $
--            SPT.proc name args))
--         SPT.stopProcess
--     return ((), Handles hin hout herr)

cmd :: (MonadUnliftIO m, MonadResource m) => String -> [String] -> Pipeline m () ()
cmd name args =
  Pipeline $ \((), Handles hin hout herr) -> do
    SPT.withProcessWait
      (SPT.setStdin (SPT.useHandleOpen hin) $
       SPT.setStdout (SPT.useHandleOpen hout) $
       SPT.setStderr (SPT.useHandleOpen herr) $
       SPT.proc name args)
      (const $ return ((), Handles hin hout herr))

captureIn :: MonadResource m => FilePath -> Pipeline m a a
captureIn fp =
  Pipeline $ \(x, Handles hin hout herr) -> do
    (_, hout') <- allocate (openFile fp WriteMode) hClose
    return (x, Handles hin hout' herr)

(|>>) :: MonadResource m => Pipeline m i o -> FilePath -> Pipeline m i o
p |>> fp = captureIn fp >>> p

ls :: (MonadUnliftIO m, MonadResource m) => Pipeline m () ()
ls = cmd "ls" ["--color=auto"]

testPipe :: (MonadUnliftIO m, MonadResource m) => Pipeline m () ()
testPipe = ls |> cmd "cat" []

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

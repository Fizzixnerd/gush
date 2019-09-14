{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib
    (
    ) where

import qualified Data.Conduit.Combinators as CC
import Conduit
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as BS
import Data.Conduit.Shell (run, shell, Segment, ($|), ProcessType, conduit)
import Data.Conduit.Shell.Segments (ignore, texts)
import Data.Conduit.Shell.PATH hiding (ls, emacs)
import qualified Data.Conduit.Shell.PATH as PATH
import System.FilePath.Glob (Pattern, globDir1)
import Control.Monad.IO.Class
import Control.Monad (void, join)
import Control.Applicative ((<|>))
import Data.Text (unpack)
import System.IO (withBinaryFile, IOMode(..), Handle)
import qualified Control.Category as Cat
import qualified Control.Arrow as Arr

-- in out err
data Handles = Handles Handle Handle Handle

newtype Pipeline m i o =
  PipelineConduit (ConduitT (i, Handles) (o, Handles) m ())

instance Monad m => Cat.Category (Pipeline m) where
  id = PipelineConduit $ CC.mapM return
  (PipelineConduit f) . (PipelineConduit g) = PipelineConduit (g `fuseUpstream` f)

instance Monad m => Arr.Arrow (Pipeline m) where
  arr f = PipelineConduit $ CC.mapM (return . \(x, handles) -> (f x, handles))
  first (PipelineConduit f) = 

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

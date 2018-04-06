{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Dependencies (servedDependencies)
import Types (AppM)
import Types.Env (Env (..))

import Web.Routes.Nested (textOnly)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, runApplicationT)
import Network.HTTP.Types (status404)
import Data.Singleton.Class (runSingleton)
import Data.Time.Clock (secondsToDiffTime)
import qualified Data.TimeMap as TimeMap
import Control.Monad (void, forM_)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control.Aligned (liftBaseWith)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TMapMVar.Hash as TMapMVar



server :: Int -> AppM ()
server port = do
  -- auth token expiring checker - FIXME use a cassandra database instead probably
  env@Env{envAuthTokens,envAuthTokenExpire} <- ask
  liftIO $ void $ async $ do
    xs <- flip TimeMap.takeFromNow envAuthTokens $ fromRational $ toRational $ secondsToDiffTime $
      let minute = 60
          hour = 60 * minute
          day = 24 * hour
      in  2 * minute -- FIXME expire after a day?
    atomically $ forM_ xs $ \(authToken,userId) ->
      TMapMVar.insert envAuthTokenExpire authToken ()
    threadDelay $
      let second = 10 ^ 6
          minute = second * 60
      in  minute

  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    dependencies <- runSingleton <$> runInBase servedDependencies
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer dependencies defApp
    runEnv port server'


defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []

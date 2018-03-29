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

import Web.Routes.Nested (textOnly)
import Network.Wai.Handler.Warp (runEnv)
import Network.Wai.Trans (ApplicationT, runApplicationT)
import Network.HTTP.Types (status404)
import Data.Singleton.Class (runSingleton)
import Control.Monad.Trans.Control.Aligned (liftBaseWith)



server :: Int -> AppM ()
server port =
  -- HTTP Server
  liftBaseWith $ \runInBase -> do
    dependencies <- runSingleton <$> runInBase servedDependencies
    server' <- fmap runSingleton $ runInBase $ runApplicationT $
      httpServer dependencies defApp
    runEnv port server'


defApp :: ApplicationT AppM
defApp _ respond = respond $ textOnly "404" status404 []

module Server.Dependencies where

import Types (AppM)

import Web.Routes.Nested (RouterT)
import Web.Dependencies.Sparrow (Server, serveDependencies, unpackServer, SparrowServerT)
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies = do
  undefined



servedDependencies :: AppM (RouterT (MiddlewareT AppM) sec AppM ())
servedDependencies = serveDependencies dependencies

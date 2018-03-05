{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , NamedFieldPuns
  #-}

module Types.Env where

import Types.Keys (Keys)
import LocalCooking.Auth (SessionID)
import Database (Users, initialUsers)

import Data.Word (Word64)
import qualified Data.Text.Encoding as T
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (..))
import Data.Default (Default (..))
import qualified Data.Strict.Maybe as Strict
import Data.Acid (AcidState, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.STM (TVar, newTVar, atomically)
import Crypto.Saltine.Core.Box (Nonce, newNonce)
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)


data Database = Database
  { dbUsers :: AcidState Users
  }

instance Default Database where
  def = Database
    { dbUsers = unsafePerformIO $! openLocalState initialUsers
    }


newtype LocalCookingSubsPerSession = LocalCookingSubsPerSession
  { koreDEXSubsOrders :: Maybe (Async ())
  }

instance Default LocalCookingSubsPerSession where
  def = LocalCookingSubsPerSession
    { koreDEXSubsOrders = Nothing
    }


cancelAllSubs :: LocalCookingSubsPerSession -> IO ()
cancelAllSubs LocalCookingSubsPerSession{koreDEXSubsOrders} =
  case koreDEXSubsOrders of
    Nothing -> pure ()
    Just th -> cancel th


data Threads = Threads
  { thSubs :: TVar (HashMap SessionID LocalCookingSubsPerSession)
  }

instance Default Threads where
  def = unsafePerformIO defThreads


defThreads :: IO Threads
defThreads = do
  thSubs <- atomically $ newTVar HashMap.empty
  pure Threads{thSubs}

data Managers = Managers
  { managersFacebook :: Manager
  }

instance Default Managers where
  def = unsafePerformIO defManagers

defManagers :: IO Managers
defManagers = do
  managersFacebook <- newTlsManager -- FIXME could bug out from facebook booting us
  pure Managers
    { managersFacebook
    }


data Development = Development
  { devCacheBuster :: Nonce
  }

instance Default Development where
  def = unsafePerformIO defDevelopment

defDevelopment :: IO Development
defDevelopment = do
  devCacheBuster <- newNonce
  pure Development
    { devCacheBuster
    }

isDevelopment :: Env -> Bool
isDevelopment Env{envDevelopment} = case envDevelopment of
  Nothing -> False
  Just _ -> True


data Env = Env
  { envDatabase    :: Database
  , envThreads     :: Threads
  , envHostname    :: URIAuth
  , envSMTPHost    :: URIAuthHost
  , envDevelopment :: Maybe Development
  , envTls         :: Bool
  , envKeys        :: Keys
  , envManagers    :: Managers
  }

instance Default Env where
  def = Env
    { envDatabase    = def
    , envThreads     = def
    , envHostname    = URIAuth Strict.Nothing Localhost (Strict.Just 3000)
    , envSMTPHost    = Localhost
    , envDevelopment = def
    , envTls         = False
    , envKeys        = error "No access to secret keys in default environment"
    , envManagers    = def
    }


releaseEnv :: Env -> IO ()
releaseEnv Env{envDatabase = Database{dbUsers}} =
  createCheckpointAndClose dbUsers

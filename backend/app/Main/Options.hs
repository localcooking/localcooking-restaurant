{-# LANGUAGE
    NamedFieldPuns
  , OverloadedLists
  , QuasiQuotes
  , OverloadedStrings
  #-}

module Main.Options where

import Types.Env (Env (..), Database (..), defThreads, defManagers)
import Database (initialUsers)

import Options.Applicative (Parser, strOption, option, switch, auto, long, help, value, showDefault)
import Data.Attoparsec.Text (parseOnly)
import Data.Attoparsec.Path (absFilePath)
import Data.URI.Auth (parseURIAuth, URIAuth (..))
import Data.URI.Auth.Host (parseURIAuthHost)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Strict.Maybe as Strict
import Data.Acid (openLocalStateFrom)
import Control.Monad (unless)
import Control.Concurrent.STM (newTVar, atomically)
import Control.Logging (errorL)
import Path (toFilePath, parent, relfile, (</>))
import System.Directory (doesDirectoryExist, createDirectory)
import Foreign.C.Types (CTime (..))


data ArgsImpl = ArgsImpl
  { argsImplSecretKey  :: FilePath
  , argsImplStore      :: FilePath
  , argsImplHostname   :: String
  , argsImplPublicPort :: Int
  , argsImplSMTPHost   :: String
  , argsImplProduction :: Bool
  }


args :: String -> Parser ArgsImpl
args username = ArgsImpl
             <$> parseSecretKey
             <*> parseStore
             <*> parseHostname
             <*> parsePublicPort
             <*> parseSMTPHost
             <*> parseProduction
  where
    parseSecretKey = strOption $
      long "secret-key" <> help "File path to the properly accessible secrets file, containing the PayPal API tokens, etc."
        <> value ("/home/" ++ username ++ "/.localcooking/secret") <> showDefault
    parseStore = strOption $
      long "store" <> help "File path to the properly accessible directory, containing the database"
        <> value ("/home/" ++ username ++ "/.localcooking/store/") <> showDefault
    parseHostname = strOption $
      long "host" <> help "Bound name & port of the server (for hyperlinks)"
        <> value "localhost:3000" <> showDefault
    parsePublicPort = option auto $
      long "public-port" <> help "Publically accessible port of the service, if different from the bound port (i.e. 80)"
        <> value 0 <> showDefault
    parseSMTPHost = strOption $
      long "smtp-host" <> help "Hostname of the SMTP outgoing mail server"
        <> value "localhost" <> showDefault
    parseProduction = switch $
      long "production" <> help "Run the server in production-mode (less logging)"


mkEnv :: ArgsImpl -> IO (Env, Int)
mkEnv
  ArgsImpl
    { argsImplSecretKey
    , argsImplStore
    , argsImplHostname
    , argsImplPublicPort
    , argsImplSMTPHost
    , argsImplProduction
    } = do
  envKeys <- case parseOnly absFilePath (T.pack argsImplSecretKey) of
    Left e -> errorL $ "Secret key path not absolute: " <> T.pack e
    Right f -> do
      exists <- doesDirectoryExist $ toFilePath $ parent f
      unless exists $ createDirectory $ toFilePath $ parent f
      x <- LBS.readFile (toFilePath f)
      case Aeson.decode x of
        Nothing -> errorL "Secret key file contents cannot be parsed"
        Just y -> pure y
  (envHostname, boundPort) <- case parseOnly parseURIAuth (T.pack argsImplHostname) of
    Left e -> errorL $ "Can't parse hostname: " <> T.pack e
    Right (URIAuth a h mPort) -> pure
      ( URIAuth a h ( if argsImplPublicPort == 80
                      then Strict.Nothing
                      else Strict.Just $ fromIntegral argsImplPublicPort
                    )
      , case mPort of
          Strict.Nothing -> 80
          Strict.Just p -> p
      )
  envSMTPHost <- case parseOnly parseURIAuthHost (T.pack argsImplSMTPHost) of
    Left e -> errorL $ "Can't parse SMTP host: " <> T.pack e
    Right a -> pure a
  envDatabase <- do
    dbUsers <- openLocalStateFrom argsImplStore initialUsers
    pure Database
      { dbUsers
      }

  putStrLn $ unlines
    [ "Starting server with environment:"
    , " - hostname: " <> argsImplHostname
    , " - public port: " <> show argsImplPublicPort
    , " - database location: " <> argsImplStore
    , " - secret key location: " <> argsImplSecretKey
    ]

  envThreads <- defThreads
  envManagers <- defManagers

  pure
    ( Env
      { envHostname
      , envSMTPHost
      , envDatabase
      , envThreads
      , envDevelopment = not argsImplProduction
      , envKeys
      , envManagers
      }
    , fromIntegral boundPort
    )

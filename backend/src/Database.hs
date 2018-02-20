{-# LANGUAGE
    DeriveDataTypeable
  , TemplateHaskell
  , GeneralizedNewtypeDeriving
  , TypeFamilies
  , NamedFieldPuns
  , RecordWildCards
  , OverloadedStrings
  #-}

module Database where

import LocalCooking.Auth (UserID)

import Data.Acid (makeAcidic, Update, Query)
import Data.SafeCopy (SafeCopy (..), base, deriveSafeCopy, safeGet, contain)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text.Base64 (base64)
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Text.Encoding as T
import Data.Data (Data, Typeable)
import Data.IxSet (IxSet, Indexable (..), ixSet, ixGen, Proxy (..), (@=), getOne)
import qualified Data.IxSet as IxSet
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Monad.State (put, get)
import Control.Monad.Reader (ask)
import Crypto.Saltine.Core.Box (Nonce)
import qualified Crypto.Saltine.Class as NaCl


newtype Username = Username {getUsername :: Text}
  deriving (Eq, Ord, Data, Typeable, FromJSON, ToJSON, Show)

$(deriveSafeCopy 0 'base ''Username)

newtype Email = Email {getEmail :: Text}
  deriving (Eq, Ord, Data, Typeable, FromJSON, ToJSON, Show)

$(deriveSafeCopy 0 'base ''Email)

newtype Salt = Salt {getSalt :: Nonce}
  deriving (Eq, Ord, Data, Typeable)

instance Show Salt where
  show = show . T.decodeUtf8 . BS64.encode . NaCl.encode . getSalt

salt :: Atto.Parser Salt
salt = do
  s <- base64
  case BS64.decode $ T.encodeUtf8 s of
    Left e -> fail $ "Not a Salt: " ++ e
    Right s' -> case NaCl.decode s' of
      Nothing -> fail "Not a Salt"
      Just x -> pure (Salt x)

instance FromJSON Salt where
  parseJSON = attoAeson salt
  -- parseJSON (String x) = case BS64.decode (T.encodeUtf8 x) of
  --   Left e -> fail e
  --   Right x -> pure (Salt x)
  -- parseJSON x = typeMismatch "Salt" x

instance ToJSON Salt where
  toJSON (Salt x) =
    String $ T.decodeUtf8 $ BS64.encode $ NaCl.encode x

instance SafeCopy Salt where
  putCopy (Salt x) = putCopy (NaCl.encode x)
  getCopy = contain $ do
    x <- safeGet
    case NaCl.decode x of
      Nothing -> fail "Not a Salt"
      Just x' -> pure (Salt x')


data User = User
  { userUsername :: Username
  , userEmail    :: Email
  , userSalt     :: Salt
  , userID       :: UserID
  } deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''User)


instance Indexable User where
  empty = ixSet
    [ ixGen (Proxy :: Proxy Username)
    , ixGen (Proxy :: Proxy Email)
    , ixGen (Proxy :: Proxy Salt)
    , ixGen (Proxy :: Proxy UserID)
    ]

instance FromJSON User where
  parseJSON (Object o) = do
    userID <- o .: "userID"
    userSalt <- o .: "salt"
    userUsername <- o .: "username"
    userEmail <- o .: "email"
    pure User
      {userID, userSalt, userUsername, userEmail}
  parseJSON x = typeMismatch "User" x

instance ToJSON User where
  toJSON User{..} = object
    [ "username" .= userUsername
    , "email" .= userEmail
    , "salt" .= userSalt
    , "userID" .= userID
    ]


newtype Users = Users {getUsers :: IxSet User}
  deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Users)

initialUsers :: Users
initialUsers = Users IxSet.empty


data LocalCookingDBInsertUserError
  = LocalCookingDBInsertUserErrorSaltExists
  | LocalCookingDBInsertUserErrorUsernameExists
  | LocalCookingDBInsertUserErrorUserIDExists
  deriving (Eq, Ord, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''LocalCookingDBInsertUserError)



insertUser :: User -> Update Users (Maybe LocalCookingDBInsertUserError)
insertUser u = do
  Users us <- get
  let withSalt = us @= userSalt u
      withUsername = us @= userUsername u
      withUserID = us @= userID u
  if not (IxSet.null withSalt)
    then pure $ Just LocalCookingDBInsertUserErrorSaltExists
    else if not (IxSet.null withUsername)
    then pure $ Just LocalCookingDBInsertUserErrorUsernameExists
    else if not (IxSet.null withUserID)
    then pure $ Just LocalCookingDBInsertUserErrorUserIDExists
    else do
      put (Users (IxSet.updateIx (userUsername u) u us))
      pure Nothing

getUserSalt :: Username -> Query Users (Maybe Salt)
getUserSalt username = do
  Users us <- ask
  pure (userSalt <$> getOne (us @= username))

getAllUsers :: Query Users Users
getAllUsers = ask

getUserDetails :: UserID -> Query Users (Maybe User)
getUserDetails userID = do
  Users us <- ask
  pure (getOne (us @= userID))

isUniqueSalt :: Salt -> Query Users Bool
isUniqueSalt salt' = do
  Users us <- ask
  pure $ isNothing $ getOne $ us @= salt'

isUniqueUsername :: Username -> Query Users Bool
isUniqueUsername username = do
  Users us <- ask
  pure $ isNothing $ getOne $ us @= username

-- getUserPassword :: Username -> Query Users (Maybe Password)
-- getUserPassword username = do
--   Users us <- ask
--   pure (userPassword <$> getOne (us @= username))


$(makeAcidic ''Users ['insertUser,'getUserSalt,'getUserDetails,'getAllUsers,'isUniqueSalt,'isUniqueUsername])

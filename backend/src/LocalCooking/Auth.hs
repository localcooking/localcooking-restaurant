{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveDataTypeable
  #-}

module LocalCooking.Auth where

import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Hashable (Hashable)
import Data.Data (Data, Typeable, Data, Typeable)
import qualified Data.UUID as UUID
import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text.Base64 (base64)
import Data.Char (isAlphaNum)
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.Aeson.Attoparsec (attoAeson)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.SafeCopy (SafeCopy (..), safeGet, contain)
import Crypto.Saltine.Core.Sign (PublicKey, signOpen)
import Crypto.Saltine.Core.Box (Nonce, newNonce)
import qualified Crypto.Saltine.Class as NaCl
import Test.QuickCheck (Arbitrary (..))
import System.IO.Unsafe (unsafePerformIO)


newtype SessionID = SessionID {getSessionID :: UUID}
  deriving (Eq, Ord, Hashable, Data, Typeable)

instance Arbitrary SessionID where
  arbitrary = pure $ unsafePerformIO $ SessionID <$> nextRandom

sessionID :: Atto.Parser SessionID
sessionID = do
  t <- Atto.takeWhile1 (\c -> isAlphaNum c || c == '-')
  case UUID.fromText t of
    Nothing -> fail "SessionID UUID parsing failed"
    Just u -> pure (SessionID u)

instance Show SessionID where
  show = UUID.toString . getSessionID

instance ToJSON SessionID where
  toJSON = toJSON . UUID.toText . getSessionID

instance FromJSON SessionID where
  parseJSON = attoAeson sessionID


newtype ChallengeID = ChallengeID {getChallengeID :: Nonce}
  deriving (Eq, Ord, Hashable, Data, Typeable)

instance Arbitrary ChallengeID where
  arbitrary = pure $ unsafePerformIO $ ChallengeID <$> newNonce

challengeID :: Atto.Parser ChallengeID
challengeID = do
  xs <- base64
  case BS64.decode (T.encodeUtf8 xs) of
    Left e -> fail $ "ChallengeID Base64 decoding error: " ++ e
    Right x
      | BS.length x == 24 -> case NaCl.decode x of
          Nothing -> fail "ChallengeID couldn't be decoded"
          Just n -> pure (ChallengeID n)
      | otherwise -> fail $ "ChallengeID isn't 24 bytes: " ++ show (BS.length x) ++ ", " ++ show x

instance Show ChallengeID where
  show = T.unpack . T.decodeUtf8 . BS64.encode . NaCl.encode . getChallengeID

instance ToJSON ChallengeID where
  toJSON = toJSON . T.decodeUtf8 . BS64.encode . NaCl.encode . getChallengeID

instance FromJSON ChallengeID where
  parseJSON = attoAeson challengeID


newtype SignedChallenge = SignedChallenge {getSignedChallenge :: ByteString}
  deriving (Eq, Ord, Hashable, Data, Typeable)


instance Show SignedChallenge where
  show = T.unpack . T.decodeUtf8 . BS64.encode . getSignedChallenge

instance ToJSON SignedChallenge where
  toJSON = toJSON . T.decodeUtf8 . BS64.encode . getSignedChallenge


signedChallenge :: Atto.Parser SignedChallenge
signedChallenge = do
  xs <- base64
  case BS64.decode (T.encodeUtf8 xs) of
    Left e -> fail $ "SignedChallenge Base64 decoding error: " ++ e
    Right x -> pure (SignedChallenge x)

instance FromJSON SignedChallenge where
  parseJSON = attoAeson signedChallenge


newtype UserID = UserID {getUserID :: PublicKey}
  deriving (Eq, Ord, Hashable, Data, Typeable)

userID :: Atto.Parser UserID
userID = do
  xs <- base64
  case BS64.decode (T.encodeUtf8 xs) of
    Left e -> fail $ "UserID Base64 decoding error: " ++ e
    Right x -> case NaCl.decode x of
      Nothing -> fail "UserID couldn't be decoded"
      Just p -> pure (UserID p)

instance Show UserID where
  show = T.unpack . T.decodeUtf8 . BS64.encode . NaCl.encode . getUserID

instance ToJSON UserID where
  toJSON = toJSON . T.decodeUtf8 . BS64.encode . NaCl.encode . getUserID

instance FromJSON UserID where
  parseJSON = attoAeson userID

instance SafeCopy UserID where
  putCopy (UserID x) = putCopy (NaCl.encode x)
  getCopy = contain $ do
    x <- safeGet
    case NaCl.decode x of
      Nothing -> fail "Couldn't decode UserID"
      Just u -> pure (UserID u)



verifySignedChallenge :: UserID -> SignedChallenge -> Maybe ChallengeID
verifySignedChallenge (UserID u) (SignedChallenge s) = ChallengeID <$> (NaCl.decode =<< signOpen u s)

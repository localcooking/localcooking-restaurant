module LocalCooking.Auth where

import Prelude
import Data.UUID (UUID, parseUUID)
import Data.ArrayBuffer.Base64 (encodeBase64, decodeBase64)
import Data.Argonaut (class EncodeJson, encodeJson, class DecodeJson, decodeJson, fail)
import Data.Maybe (Maybe (..))
import Crypt.NaCl.Types (SignPublicKey, SignSecretKey, SignedMessage, Nonce)
import Crypt.NaCl.Sign (sign)
import Unsafe.Coerce (unsafeCoerce)


newtype SessionID = SessionID UUID

instance showSessionID :: Show SessionID where
  show (SessionID x) = show x

instance encodeJsonSessionID :: EncodeJson SessionID where
  encodeJson (SessionID x) = encodeJson (show x)

instance decodeJsonSessionID :: DecodeJson SessionID where
  decodeJson json = do
    s <- decodeJson json
    case parseUUID s of
      Nothing -> fail "Not a UUID"
      Just x -> pure (SessionID x)



newtype ChallengeID = ChallengeID Nonce

instance encodeJsonChallengeID :: EncodeJson ChallengeID where
  encodeJson (ChallengeID x) = encodeJson $ encodeBase64 $ unsafeCoerce x

instance decodeJsonChallengeID :: DecodeJson ChallengeID where
  decodeJson json = do
    s <- decodeJson json
    case decodeBase64 s of
      Nothing -> fail "Not a ChallengeID"
      Just x -> pure $ ChallengeID $ unsafeCoerce x


newtype UserID = UserID SignPublicKey

instance encodeJsonUserID :: EncodeJson UserID where
  encodeJson (UserID x) = encodeJson $ encodeBase64 $ unsafeCoerce x

instance decodeJsonUserID :: DecodeJson UserID where
  decodeJson json = do
    s <- decodeJson json
    case decodeBase64 s of
      Nothing -> fail "Not a UserID"
      Just x -> pure $ UserID $ unsafeCoerce x


newtype SignedChallenge = SignedChallenge SignedMessage

instance encodeJsonSignedChallenge :: EncodeJson SignedChallenge where
  encodeJson (SignedChallenge x) = encodeJson $ encodeBase64 $ unsafeCoerce x

instance decodeJsonSignedChallenge :: DecodeJson SignedChallenge where
  decodeJson json = do
    s <- decodeJson json
    case decodeBase64 s of
      Nothing -> fail "Not a SignedChallenge"
      Just x -> pure $ SignedChallenge $ unsafeCoerce x


signChallenge :: SignSecretKey -> ChallengeID -> SignedChallenge
signChallenge sk (ChallengeID n) = SignedChallenge (sign (unsafeCoerce n) sk)

{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  #-}

module Login.Facebook where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Aeson (FromJSON (..), (.:), Value (..))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))


data FacebookLoginReturn a
  = FacebookLoginReturnBad
      { facebookLoginBadErrorCode :: BS.ByteString
      , facebookLoginBadErrorMessage :: BS.ByteString
      }
  | FacebookLoginReturnGood
      { facebookLoginGoodCode :: T.Text
      , facebookLoginGoodState :: a
      }
  | FacebookLoginReturnDenied
      { facebookLoginDeniedErrorDescription :: BS.ByteString
      }
  deriving (Show)


data FacebookLoginGetToken
  = FacebookLoginGetToken
    { facebookLoginGetTokenAccessToken :: T.Text
    , facebookLoginGetTokenTokenType :: T.Text
    , facebookLoginGetTokenExpiresIn :: Int
    }
  | FacebookLoginGetTokenError
    { facebookLoginGetTokenErrorMessage :: T.Text
    , facebookLoginGetTokenErrorType :: T.Text
    , facebookLoginGetTokenErrorCode :: Int
    , facebookLoginGetTokenErrorFbTraceID :: T.Text
    }
  deriving (Show)

instance FromJSON FacebookLoginGetToken where
  parseJSON (Object o) = do
    let good = do
          facebookLoginGetTokenAccessToken <- o .: "access_token"
          facebookLoginGetTokenTokenType <- o .: "token_type"
          facebookLoginGetTokenExpiresIn <- o .: "expires_in"
          pure FacebookLoginGetToken
            { facebookLoginGetTokenAccessToken
            , facebookLoginGetTokenTokenType
            , facebookLoginGetTokenExpiresIn
            }
        error' = do
          o' <- o .: "error"
          facebookLoginGetTokenErrorMessage <- o' .: "message"
          facebookLoginGetTokenErrorType <- o' .: "type"
          facebookLoginGetTokenErrorCode <- o' .: "code"
          facebookLoginGetTokenErrorFbTraceID <- o' .: "fbtrace_id"
          pure FacebookLoginGetTokenError
            { facebookLoginGetTokenErrorMessage
            , facebookLoginGetTokenErrorType
            , facebookLoginGetTokenErrorCode
            , facebookLoginGetTokenErrorFbTraceID
            }
    good <|> error'
  parseJSON x = typeMismatch "FacebookLoginGetToken" x


-- data FacebookLoginOrigin
--   = FacebookLoginOriginApp SiteLinks
--   | FacebookLoginOriginRegister
--       { facebookLoginOriginRegisterName :: Text
--       , facebookLoginOriginRegisterUsername :: Text
--       , facebookLoginOriginRegisterAddress :: Text
--       }


-- data FacebookLoginState = FacebookLoginState
--   { facebookLoginStateOrigin :: SiteLinks
--   , facebookLoginStateAux :: forall a. a
--   }

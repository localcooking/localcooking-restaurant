module Server.Dependencies.AuthToken where

import Login.Facebook (FacebookLoginCode)
import LocalCooking.Password (HashedPassword)
import LocalCooking.Auth (AuthToken)
import LocalCooking.Email (Email)


data AuthTokenInitIn
  = AuthTokenInitInLogin
    { authTokenInitInLoginEmail :: Email
    , authTokenInitInLoginPassword :: HashedPassword
    }
  | AuthTokenInitInFacebookCode
    { authTokenInitInFacebookCode :: FacebookLoginCode
    }
  | AuthTokenInitInExists
    { authTokenInitInExists :: AuthToken
    }


data AuthTokenInitOut
  = AuthTokenInitOutSuccess AuthToken
  | AuthTokenInitOutFailure


data AuthTokenDeltaIn
  = AuthTokenDeltaInLogout -- TODO plus AuthToken...? Tokens are --more-- mutually unique than SIDs?
    -- a session can die, but store the AuthToken in local storage and attempt to use later -
    -- login's discontinuity and session's discontinuity mutually overlay.


data AuthTokenDeltaOut
  = AuthTokenDeltaOutNew AuthToken
  | AuthTokenDeltaOutRevoked -- remotely logged out

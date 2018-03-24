module LocalCooking.Password where

import Data.ByteString (ByteString)


newtype HashedPassword = HashedPassword
  { getPassword :: ByteString
  } deriving (Eq)

module LocalCooking.Email where

import Data.Text (Text)


newtype Email = Email
  { getEmail :: Text
  } deriving (Eq)


-- TODO parsers, verifiers

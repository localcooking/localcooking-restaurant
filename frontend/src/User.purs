module User where

import LocalCooking.User (class UserDetails)
import LocalCooking.Common.User.Role (UserRole)

import Data.Maybe (Maybe)
import Text.Email.Validate (EmailAddress)



data PreUserDetails = PreUserDetails (Maybe EmailAddress) (Array UserRole)


newtype UserDetails = UserDetails
  { email :: EmailAddress
  , roles :: Array UserRole
  }


instance userDetailsUserDetails :: UserDetails UserDetails where
  getEmailAddress (UserDetails {email}) = email
  getUserRoles (UserDetails {roles}) = roles

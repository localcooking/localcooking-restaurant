module User where

import LocalCooking.User (class UserDetails)

import Text.Email.Validate (EmailAddress)


newtype UserDetails = UserDetails
  { email :: EmailAddress
  }


instance userDetailsUserDetails :: UserDetails UserDetails where
  getEmailAddress (UserDetails {email}) = email

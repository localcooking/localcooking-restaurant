module Error where


-- data CustomerError
--   = CustomerSaveFailed
--   | CustomerSaveSuccess


data SiteError
  -- = SiteErrorCustomer CustomerError


printSiteError :: SiteError -> String
printSiteError e = "" -- case e of
  -- SiteErrorCustomer cust -> case cust of
  --   CustomerSaveFailed -> "Internal error - couldn't save customer details"
  --   CustomerSaveSuccess -> "Customer details saved."

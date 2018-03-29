module Facebook.Call where

import Types.Env (env)

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.URI (URI (..), HierarchicalPart (..), Scheme (..), Port (..), Host (..), Authority (..), Query (..))
import Data.URI.URI as URI
import Data.List (List (..))
import Data.Path.Pathy (rootDir, dir, file, (</>))
import Data.Argonaut (class EncodeJson, encodeJson)


newtype FacebookLoginLink a
  = FacebookLoginLink
    { redirectURL :: URI
    , state :: a
    }

facebookLoginLinkToURI :: forall a. EncodeJson a => FacebookLoginLink a -> URI
facebookLoginLinkToURI (FacebookLoginLink {redirectURL,state}) =
  URI
    (Just $ Scheme "https")
    ( HierarchicalPart
      (Just $ Authority Nothing [Tuple (NameAddress "www.facebook.com") Nothing])
      (Just $ Right $ rootDir </> dir "v2.12" </> dir "dialog" </> file "oauth")
    )
    ( Just $ Query
      $ Cons
        (Tuple "client_id" $ Just env.facebookClientID)
      $ Cons
        (Tuple "redirect_uri" $ Just $ URI.print redirectURL)
      $ Cons
        (Tuple "state" $ Just $ show $ encodeJson state)
        Nil
    )
    Nothing

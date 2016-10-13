{-|
Module      : Servant.API.DB
Description : Type level DSL for describing API to DB
Portability : Portable

The idea of package is to provide <http://haskell-servant.readthedocs.io/en/stable/ servant>-like
DSL for specifying an API for functions stored in RDBMS.

@
data RegisterUser = RegisterUser {
  userName :: Text
, userPassword :: Text
, userPhone :: Phone
, userEmail :: Email
}

type API =
       ArgNamed "user" RegisterUser
    :> ArgNamed "isAdmin" Bool
    :> Procedure "registerUser" (Maybe (Only UserId))
  :<|> ArgPos UserId
    :> Procedure "getUser" (Maybe User)
  :<|> Procedure "listUsers" [User]
@

The library adds three custom combinators:
* `ArgNamed name a` - named argument of stored function of type `a`.

* `ArgPos a` - unamed argument of stored function of type `a`.

* `Procedure name a` - named stored function with return type `a`.

Related libraries:

* <https://github.com/NCrashed/servant-db-postgresql servant-db-postgresql> -
    derives client for PostgreSQL with <http://hackage.haskell.org/package/postgresql-query postgresql-query> library.

-}
module Servant.API.DB(
    module Reexport
  ) where

import           Servant.API              as Reexport ((:<|>) (..), (:>))
import           Servant.API.DB.Argument  as Reexport
import           Servant.API.DB.Procedure as Reexport

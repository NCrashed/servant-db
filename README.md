servant-db
==========

The idea of package is to provide [servant](http://haskell-servant.readthedocs.io/en/stable/)-like 
DSL for specifying an API for functions stored in RDBMS. 

``` haskell
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
```

The library adds three custom combinators:
* `ArgNamed name a` - named argument of stored function of type `a`.

* `ArgPos a` - unamed argument of stored function of type `a`.

* `Procedure name a` - named stored function with return type `a`. 

Related libraries:

* [servant-db-postgresql](https://github.com/NCrashed/servant-db-postgresql) - 
    derives client for PostgreSQL with [postgresql-query](postgresql-query) library.

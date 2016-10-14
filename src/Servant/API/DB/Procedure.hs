{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Servant.API.DB.Procedure
Description : Endpoint for DB stored function
Portability : Portable
-}
module Servant.API.DB.Procedure(
    Procedure
  ) where

import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits

-- | Endpoint for SQL procedure.
--
-- Example, stored function that returns current time in seconds:
--
-- >>> type SelectTime = Procedure "time" Integer
--
-- Example, stored function that squares given 'Int':
--
-- >>> type SquareProcedure = Arg "a" Int :> Procedure "square" Int
data Procedure (name :: Symbol) a
  deriving (Generic, Typeable)


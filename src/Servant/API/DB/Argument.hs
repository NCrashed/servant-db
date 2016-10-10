{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Servant.API.DB.Argument
Description : Argument for DB stored functions
Portability : Portable
-}
module Servant.API.DB.Argument(
    ArgPos
  , ArgNamed
  ) where

import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits

-- | Positional argument of DB stored function with attached type info.
--
-- >>> type SquareProcedure = ArgPos Int :> Procedure "square" Int
data ArgPos a
  deriving (Generic, Typeable)

-- | Named argument of DB stored function with attached name and type info.
--
-- >>> type SquareProcedure = ArgNamed "a" Int :> Procedure "square" Int
data ArgNamed (name :: Symbol) a
  deriving (Generic, Typeable)

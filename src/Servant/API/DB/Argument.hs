{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Servant.API.DB.Argument
Description : Argument for DB stored functions
Portability : Portable
-}
module Servant.API.DB.Argument(
    Arg
  ) where

import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits

-- | DB procedure argument with name and type.
--
-- >>> type SquareProcedure = Arg "a" Int :> Procedure "square" Int
data Arg (name :: Symbol) a
  deriving (Generic, Typeable)

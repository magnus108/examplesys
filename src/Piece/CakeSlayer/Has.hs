{-# LANGUAGE DataKinds #-}

module Piece.CakeSlayer.Has
  ( Has (..),
    grab,
    Field (..),
  )
where

import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)

class Has field env where
  obtain :: env -> field

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}

newtype Field (s :: Symbol) env = Field
  { unField :: env
  }

instance forall s f env. (HasField s env f) => Has f (Field s env) where
  obtain :: Field s env -> f
  obtain = getField @s . unField
  {-# INLINE obtain #-}

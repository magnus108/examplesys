module Piece.App.Error
  ( WithError,
    AppError (..),
    UserError (..),
    As (..),
  )
where

import qualified Piece.CakeSlayer.Error as CakeSlayer

type WithError e m = CakeSlayer.WithError e m

data UserError = NotFound
  deriving stock (Show, Eq)

data AppError = AppUserError UserError
  deriving stock (Show, Eq)

instance As AppError UserError where
  as = AppUserError
  match (AppUserError x) = Just x
  match _ = Nothing

instance As AppError AppError where
  as = id
  match = Just

class As s a where
  as :: a -> s
  match :: s -> Maybe a

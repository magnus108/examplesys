{-# LANGUAGE TemplateHaskell #-}

module Piece.App.Error2
  ( AppError,
  )
where

import qualified Control.Lens.TH as Lens

data UserError = NotFound
  deriving stock (Show, Eq)

Lens.makeClassyPrisms ''UserError

data AppError = AppUserError UserError
  deriving stock (Show)

Lens.makeClassyPrisms ''AppError

instance AsUserError AppError where
  _UserError = _AppUserError . _UserError

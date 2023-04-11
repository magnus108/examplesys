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

data AppError = AppUserError UserError | GGerr
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

-- | Map 'AppError' into a HTTP error code.
-- toHttpError :: CakeSlayer.ErrorWithSource AppError -> Servant.ServerError
-- toHttpError CakeSlayer.ErrorWithSource{..} = case errorWithSourceType of
--   NotFound               -> err404
--  ServerError msg        -> err500 { errBody = encodeUtf8 msg }
-- NotAllowed msg         -> err401 { errBody = encodeUtf8 msg }
-- Invalid msg            -> err417 { errBody = encodeUtf8 msg }
-- MissingHeader name     -> err401 { errBody = toLazy $ "Header not found: " <> foldedCase name }
-- HeaderDecodeError name -> err401 { errBody = encodeUtf8 $ "Unable to decode header: " <> name }
-- DbError e              -> err500 { errBody = encodeUtf8 e }
-- DbNamedError e         -> err500 { errBody = show e }

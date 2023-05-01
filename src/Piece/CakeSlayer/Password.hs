module Piece.CakeSlayer.Password
  ( PasswordHash (unPasswordHash),
    PasswordPlainText (..),
    unsafePwdHash,
    mkPasswordHashWithPolicy,
    mkPasswordHash,
    verifyPassword,
  )
where

import qualified Crypto.BCrypt as BC
import Data.Aeson (FromJSON, ToJSON)

newtype PasswordHash = PasswordHash
  { unPasswordHash :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

unsafePwdHash :: Text -> PasswordHash
unsafePwdHash = PasswordHash

newtype PasswordPlainText = PasswordPlainText
  { unPasswordPlainText :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

mkPasswordHashWithPolicy ::
  forall m.
  (MonadIO m) =>
  BC.HashingPolicy ->
  PasswordPlainText ->
  m (Maybe PasswordHash)
mkPasswordHashWithPolicy hashPolicy password =
  PasswordHash . decodeUtf8 <<$>> hashBS
  where
    hashBS :: m (Maybe ByteString)
    hashBS =
      liftIO $
        BC.hashPasswordUsingPolicy
          hashPolicy
          (encodeUtf8 $ unPasswordPlainText password)

mkPasswordHash ::
  (MonadIO m) =>
  PasswordPlainText ->
  m (Maybe PasswordHash)
mkPasswordHash = mkPasswordHashWithPolicy BC.slowerBcryptHashingPolicy
{-# INLINE mkPasswordHash #-}

verifyPassword :: PasswordPlainText -> PasswordHash -> Bool
verifyPassword (PasswordPlainText password) (PasswordHash hash) =
  BC.validatePassword (encodeUtf8 hash) (encodeUtf8 password)
{-# INLINE verifyPassword #-}

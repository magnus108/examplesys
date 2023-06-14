{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Piece.Core.UserCreateForm
  ( User,
    user,
    form,
    toName,
    toPassword,
    toRoles,
    FormInput,
    Hash,
    MyHashable (..),
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product
import Data.Functor.Product (Product)
import Data.Generic.HKD
import Data.Text hiding (elem)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import Prelude hiding (Product)

type FormInput = Maybe

type Hash = Compose Maybe (Const String)

type User = HKD User.User (Product FormInput Hash)

user :: f String -> f Password.PasswordHash -> f [Int] -> HKD User.User f
user = build @User.User

toName :: User -> Maybe String
toName user =
  let formName = user ^. field @"name"
   in fst_ formName

toPassword :: User -> Maybe String
toPassword user =
  let formPassword = user ^. field @"password"
   in fmap (getConst) $ getCompose $ snd_ formPassword

toRoles :: User -> Maybe [Int]
toRoles user =
  let formRoles = user ^. field @"roles"
   in fst_ formRoles

fst_ :: Product g h p -> g p
fst_ (Pair x _) = x

snd_ :: Product g h p -> h p
snd_ (Pair _ y) = y

form :: String -> String -> Bool -> User
form name password admin =
  user
    (Pair (Just name) (Compose Nothing))
    (Pair Nothing (Compose (Just (Const password))))
    (Pair (if admin then Just [2] else Just []) (Compose Nothing))

class MyHashable a where
  hash :: String -> Maybe a -- IO (Maybe Password.PasswordHash)

-- instance MonadIO m => MyHashable m Hash (Password.PasswordHash) where
instance MyHashable Password.PasswordHash where
  hash x = Nothing

instance MyHashable String where
  hash x = Nothing

instance MyHashable [Int] where
  hash x = Nothing

data FormDataParser a where
  StringE :: String -> FormDataParser String
  PasswordE :: String -> FormDataParser Password.PasswordHash
  BoolE :: Bool -> FormDataParser [Int]

evalFormDataParser2 :: FormDataParser a -> Compose IO Maybe a
evalFormDataParser2 (StringE s) = Compose (return (Just s))
evalFormDataParser2 (PasswordE s) = Compose . Password.mkPasswordHash . Password.PasswordPlainText . pack $ s
evalFormDataParser2 (BoolE b) = Compose (return (Just (if b then [3] else [])))

gg = evalFormDataParser2 $ PasswordE "lol"

-- instance MonadIO m => MyHashable m (Product FormInput Hash) (Password.PasswordHash) where
--  hash x = Compose (join <$> mapM (Password.mkPasswordHash . Password.PasswordPlainText . pack) (fmap (getConst . getCompose) $ getCompose $ snd_ x))

{-
form =
  let name = "bob"
      password = "code"
      admin = True
      mus = us (Pair (Just name) (Compose Nothing)) (Pair Nothing (Compose (Just (Const "code")))) (Pair (if admin then Just [3] else Just []) (Compose Nothing))
   in mus

backToForm :: (String, Text, Bool)
backToForm =
  let formName = mus ^. field @"_name" -- Just bob
      formPassword = mus ^. field @"_password" -- Alsways nothing meaning we dont store right value
      formRoles = mus ^. field @"_roles" -- Just [1,2,3]
   in (fromMaybe "" formName, fromMaybe (pack "") formPassword, maybe False (elem 3) formRoles)
   -}

-- Form til MaybeUs
-- MaybeUs til Form
-- MaybeHashUs -> MaybeUs -> Us

-- name :: Nothing                      name :: Just String              name :: Identity String
-- password :: Just (Hash String)       password :: Nothing              password :: Identity String
-- Admin :: Nothing                     Admin :: Just [1,2]              Admin :: Identity [Int]

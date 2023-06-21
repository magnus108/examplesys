{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserCreateForm
  ( User,
    user,
    mainer,
    form,
    toName,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Control.Monad.Signatures (Pass)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Product
import Data.Functor.Product (Product)
import Data.Generic.HKD
import Data.Text hiding (elem)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User
import Prelude hiding (Product)

type User = HKD User.User FormDataExpr

toName :: User -> String
toName user =
  let formName = user ^. field @"name"
   in fromStringExpr $ formName

toPassword :: User -> String
toPassword user =
  let formPassword = user ^. field @"password"
   in fromPasswordHashExpr $ formPassword

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String
  PasswordExpr :: FormDataExpr String -> FormDataExpr Password.PasswordHash
  BoolExpr :: Bool -> FormDataExpr [Int]

user :: f String -> f Password.PasswordHash -> f [Int] -> HKD User.User f
user = build @User.User

form :: String -> String -> Bool -> User
form name password admin =
  user
    (StringExpr name)
    (PasswordExpr (StringExpr password))
    (BoolExpr admin)

fromStringExpr :: FormDataExpr String -> String
fromStringExpr (StringExpr x) = x

fromPasswordHashExpr :: FormDataExpr Password.PasswordHash -> String
fromPasswordHashExpr (PasswordExpr x) = fromStringExpr x

evalFormDataParser :: FormDataExpr a -> Compose IO Maybe a
evalFormDataParser (StringExpr s) = Compose (return (Just s))
evalFormDataParser (PasswordExpr (StringExpr s)) = Compose . Password.mkPasswordHash . Password.PasswordPlainText . pack $ s
evalFormDataParser (BoolExpr b) = Compose (return (Just (if b then [3] else [])))

data Lol a where
  StringL :: String -> Lol String
  PasswordL :: String -> Lol Password.PasswordHash
  BoolL :: Bool -> Lol [Int]

type family ConstructionParams a where
  ConstructionParams (Lol String) = String
  ConstructionParams (Lol [Int]) = Bool
  ConstructionParams (Lol Password.PasswordHash) = String

getConstructionParams :: Lol a -> ConstructionParams (Lol a)
getConstructionParams (StringL param) = param
getConstructionParams (PasswordL param) = param
getConstructionParams (BoolL param) = param

getit :: MonadIO m => Lol a -> m (Maybe a)
getit (StringL param) = return $ Just param
getit (PasswordL param) = Password.mkPasswordHash . Password.PasswordPlainText . pack $ param
getit (BoolL param) = return $ Just $ if param then [3] else []

type Loler = HKD User.User Lol

loller :: String -> String -> Bool -> Loler
loller name password admin =
  user
    (StringL name)
    (PasswordL password)
    (BoolL admin)

mainer :: IO ()
mainer = do
  let fooParam = getConstructionParams (PasswordL "hallA") :: String
  let barParam = getConstructionParams (StringL "lola") :: String
  ada <- getit (PasswordL "haalal")
  print ada
  print fooParam -- Output: 42
  print barParam -- Output: True

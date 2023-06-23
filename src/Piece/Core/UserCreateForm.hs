{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserCreateForm
  ( User,
    form,
    getFormData,
    constructData,
    toName,
    toRoles,
    toPassword,
    toRoles,
  )
where

import Control.Lens (Const (..), Identity, anyOf, (&), (.~), (^.))
import Data.Generic.HKD
import Data.Text hiding (elem)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User

type User = HKD User.User (Compose (Either ()) FormDataExpr)

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String
  PasswordExpr :: String -> FormDataExpr Password.PasswordHash
  BoolExpr :: Bool -> FormDataExpr [Int]

type family FormData a where
  FormData (FormDataExpr String) = String
  FormData (FormDataExpr [Int]) = Bool
  FormData (FormDataExpr Password.PasswordHash) = String

getFormData :: FormDataExpr a -> FormData (FormDataExpr a)
getFormData (StringExpr param) = param
getFormData (PasswordExpr param) = param
getFormData (BoolExpr param) = param

constructData :: FormDataExpr a -> Compose IO Maybe a
constructData (StringExpr param) = Compose $ return $ Just param
constructData (PasswordExpr param) = Compose $ Password.mkPasswordHash . Password.PasswordPlainText . pack $ param
constructData (BoolExpr param) = Compose $ return $ Just $ if param then [3] else []

form :: String -> String -> Bool -> User
form name password admin =
  build @User.User
    (Compose (Right (StringExpr name)))
    (Compose (Right (PasswordExpr password)))
    (Compose (Right (BoolExpr admin)))

toName :: User -> Either () String
toName user =
  let formName = user ^. field @"name"
      dd = getCompose formName
      zz = fmap getFormData dd
   in zz

toPassword :: User -> Either () String
toPassword user =
  let formName = user ^. field @"password"
      dd = getCompose formName
      zz = fmap getFormData dd
   in zz

toRoles :: User -> Either () Bool
toRoles user =
  let formName = user ^. field @"roles"
      dd = getCompose formName
      zz = fmap getFormData dd
   in zz

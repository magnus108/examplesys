{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.UserCreateForm
  ( User,
    form,
    getFormData,
    constructData,
  )
where

import Data.Generic.HKD
import Data.Text hiding (elem)
import qualified Piece.CakeSlayer.Password as Password
import qualified Piece.Core.User as User

type User = HKD User.User FormDataExpr

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
    (StringExpr name)
    (PasswordExpr password)
    (BoolExpr admin)

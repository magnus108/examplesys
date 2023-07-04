{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Piece.Core.Form.FormDataExpr
  ( FormDataExpr (..),
    FormData,
    getFormData,
    constructData,
  )
where

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String

type family FormData f a = r | r -> f a where
  FormData FormDataExpr String = String

getFormData :: FormDataExpr a -> FormData FormDataExpr a
getFormData (StringExpr param) = param

constructData :: FormDataExpr a -> a
constructData (StringExpr param) = param

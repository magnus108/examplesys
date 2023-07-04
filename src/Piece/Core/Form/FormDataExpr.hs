{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Piece.Core.Form.FormDataExpr
  ( FormDataExpr (..),
    FormData,
    getFormData,
    constructData,
    getContainer,
  )
where

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String

type family FormData a = r | r -> a where
  FormData (FormDataExpr String) = String

newtype Container a = Container {getContainer :: FormData (FormDataExpr a)}

getFormData :: FormDataExpr a -> Container a
getFormData (StringExpr param) = Container param

constructData :: FormDataExpr a -> a
constructData (StringExpr param) = param

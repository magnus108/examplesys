{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Piece.Core.Form.FormDataExpr
  ( FormDataExpr (..),
    FormData,
    Form (..),
    to,
    from,
    getFormData,
    constructData,
    getContainer,
  )
where

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String
  SelectExpr :: Int -> FormDataExpr Int

type family FormData a where
  FormData (FormDataExpr String) = String
  FormData (FormDataExpr Int) = Int

newtype Container a = Container {getContainer :: FormData (FormDataExpr a)}

getFormData :: FormDataExpr a -> Container a
getFormData (StringExpr param) = Container param
getFormData (SelectExpr param) = Container param

constructData :: FormDataExpr a -> a
constructData (StringExpr param) = param
constructData (SelectExpr param) = param

data Form a = Form
  { from :: Maybe (FormDataExpr a),
    to :: a -> FormDataExpr a
  }

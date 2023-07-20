{-# LANGUAGE GADTs #-}

module Piece.Core.Form.FormDataExpr2
  ( FormDataExpr (..),
    FormData (..),
    Form (..),
    to,
    from,
    constructData,
  )
where

data FormDataExpr a where
  StringExpr :: String -> FormDataExpr String
  SelectExpr :: Int -> FormDataExpr Int

constructData :: FormDataExpr a -> a
constructData (StringExpr param) = param
constructData (SelectExpr param) = param

data FormData
  = StringData String
  | SelectData Int

data Form a = Form
  { from :: Compose Maybe FormDataExpr a, -- FROM A FORM
    to :: a -> FormData -- TO A FORM
  }

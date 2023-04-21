module Piece.Db.ListZipper
  ( ListZipper,
    fromList,
    toList,
    backward,
    forward,
    lefts,
    rights,
    singleton,
    setFocus,
  )
where

import Control.Comonad
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Prelude hiding (fromList, lefts, rights, toList)

data ListZipper a = ListZipper [a] a [a]
  deriving stock (Eq, Show, Functor, Generic)
  deriving (Foldable, Traversable)
  deriving (FromJSON, ToJSON)

instance Comonad ListZipper where
  extract (ListZipper _ x _) = x
  duplicate a = ListZipper (shift backward) a (shift forward)
    where
      shift move = NE.tail $ iterate move a
      iterate f x = case f x of
        Just x' -> NE.cons x (iterate f x')
        Nothing -> NE.singleton x

setFocus :: a -> ListZipper a -> ListZipper a
setFocus y (ListZipper ls _ rs) = listZipper ls y rs

singleton :: a -> ListZipper a
singleton x = listZipper [] x []

listZipper :: [a] -> a -> [a] -> ListZipper a
listZipper ls x rs = ListZipper ls x rs

fromList :: [a] -> Maybe (ListZipper a)
fromList [] = Nothing
fromList (x : xs) = Just $ listZipper [] x xs

toList :: ListZipper a -> [a]
toList x = lefts x <> (extract x : rights x)

backward :: ListZipper a -> Maybe (ListZipper a)
backward (ListZipper (l : ls) a rs) = Just (listZipper ls l (a : rs))
backward (ListZipper [] _ _) = Nothing

forward :: ListZipper a -> Maybe (ListZipper a)
forward (ListZipper ls a (r : rs)) = Just (listZipper (a : ls) r rs)
forward (ListZipper _ _ []) = Nothing

lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls

rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs

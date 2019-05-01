module Item 
( Item(..)
) where

data Item = Item
  { name :: String
  } deriving (Show, Eq)
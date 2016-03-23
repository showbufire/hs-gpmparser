module Greymap (Greymap(..)) where

import qualified Data.ByteString.Lazy as L

data Greymap = Greymap {
  greyWidth :: Int
, greyHeight :: Int
, greyMax :: Int
, greyData :: L.ByteString
}

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

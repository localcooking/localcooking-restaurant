module Window where

import Prelude
import Control.Monad.Eff (Eff)


data WindowSize
  = Pager
  | Phone
  | Tablet
  | Laptop
  | Desktop



widthToWindowSize :: Int -> WindowSize
widthToWindowSize s
  | s <= 480 = Pager
  | s <= 960 = Phone
  | s <= 1280 = Tablet
  | s <= 1600 = Laptop
  | otherwise = Desktop

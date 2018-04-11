module Main where

import LocalCooking.Main (defaultMain)

import Server (server)


main :: IO ()
main = do
  defaultMain head' server
  where
    head' = "localcooking - LocalCooking.com Server"

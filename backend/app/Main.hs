{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where

import Server (server)
import Types (runAppM)
import Types.Env (Env, releaseEnv)
import Main.Options (args, mkEnv)

import Options.Applicative (execParser, info, helper, fullDesc, progDesc, header)
import Data.Monoid ((<>))
import Control.Exception.Safe (bracket)
-- import System.Posix.User (getLoginName)
import System.Environment (getEnv)
import Control.Logging (withStderrLogging)


main :: IO ()
main = do
  username <- getEnv "USER"

  let allocate :: IO (Env, Int)
      allocate = do
        as <- execParser (opts username)
        mkEnv as

      release :: (Env, Int) -> IO ()
      release (e,_) =
        releaseEnv e

  withStderrLogging $
    bracket allocate release $ \(env, port) ->
      runAppM (server port) env

  where
    opts u = info (helper <*> args u) $ fullDesc <> progDesc desc <> header head'
    desc = "Start the daemon"
    head' = "localcooking - LocalCooking.com Server"

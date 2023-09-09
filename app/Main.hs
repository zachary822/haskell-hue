{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv qualified as D
import Data.Default
import Lib.HueBridge.Discovery
import Network.Connection
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment

main :: IO ()
main = do
  D.onMissingFile (D.loadFile D.defaultConfig) mempty

  hueBridgeUsername <- getEnv "HUE_BRIDGE_USERNAME"
  bridge : _ <- discoverHueBridges defaultConfig

  let managerSettings = mkManagerSettings (def :: TLSSettings){settingDisableCertificateValidation = True} Nothing

  manager <- newManager managerSettings

  req <- parseRequest $ "https://" ++ hosttarget bridge ++ "/api/0/config"

  response <- httpLbs req manager

  print $ responseBody response

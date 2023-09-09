{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv qualified as D
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Default
import Lib.HueBridge
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

  (body :: BasicConfig) <- withResponse req manager $ \response ->
    brConsume (responseBody response) >>= throwDecode . LBS.fromChunks

  print body

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Network.Wai.Handler.Warp
import Servant
import ServantFix

-- Example
type API = Get '[ JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = return 10

main :: IO ()
main = run 8080 (serveUnder "testing" api server)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

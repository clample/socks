{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startChatServer
    , Config(..)
    ) where

import Network.WebSockets ( runServer
                          , PendingConnection
                          , acceptRequest
                          , receiveData
                          , sendTextData)
import qualified Data.Text as T
import Control.Monad (forever)

data Config = Config
              { address :: String
              , port    :: Int
              } deriving (Show, Eq)

startChatServer :: Config -> IO ()
startChatServer config =
  runServer (address config) (port config) chatServer

chatServer :: PendingConnection -> IO ()
chatServer pendingConnection = do
  connection <- acceptRequest pendingConnection
  forever $ do
    msg <- receiveData connection
    sendTextData connection $ msg `T.append` "meow"
    

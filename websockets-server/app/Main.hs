module Main where

import Lib

main :: IO ()
main = startChatServer config
  where config = Config
          { address = "127.0.0.1"
          , port = 1855}

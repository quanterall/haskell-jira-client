module Library where

import Qtility
import System.IO (putStrLn)

runMain :: IO ()
runMain = do
  putStrLn "Hello, World!"

baseUrl :: String
baseUrl = "https://quanterall.atlassian.net/"

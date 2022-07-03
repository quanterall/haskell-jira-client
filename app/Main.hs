module Main where

import qualified Library
import Qtility

-- This `main` function just delegates to the library's definition of `main`
main :: IO ()
main = Library.runMain

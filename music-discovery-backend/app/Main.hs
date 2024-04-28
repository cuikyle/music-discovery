module Main where

import qualified Server
import Prelude

-- This `main` function just delegates to the server's definition of `main`
main :: IO ()
main = Server.runMain

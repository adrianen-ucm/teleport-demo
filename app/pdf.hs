module Main where

import           Quipper  (Format (PDF), print_simple)
import           Teleport (teleport)

main :: IO ()
main = print_simple PDF teleport

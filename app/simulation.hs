module Main where

import           Quipper                                        (qinit_of_char)
import           Quipper.Libraries.Simulation.QuantumSimulation (run_generic_io)
import           System.Environment                             (getArgs)
import           Teleport                                       (protocol)

main :: IO ()
main = do
  c <- parse <$> getArgs
  (alice, bob) <- run_generic_io (0 :: Double) $ do
    qinit_of_char c >>= protocol
  putStrLn $ "Alice: " <> show alice
  putStrLn $ "Bob: " <> show bob

parse :: [String] -> Char
parse [[c]]
  | c `elem` "01+-" = c
  | otherwise = error "Only 0, 1, + or - allowed"
parse _ = error "Expected a single 0, 1, + or - as argument"

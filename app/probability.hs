module Main where

import           Data.Bifunctor                                 (Bifunctor (first))
import           Data.List                                      (nub)
import           Quipper                                        (qinit_of_char)
import           Quipper.Libraries.Simulation.QuantumSimulation (Vector (Vector),
                                                                 sim_generic)
import           System.Environment                             (getArgs)
import           Teleport                                       (protocol)

main :: IO ()
main = do
  c <- parse <$> getArgs
  let Vector ps = sim_generic (0 :: Double) $
        qinit_of_char c >>= protocol
  putStrLn $ "Alice: " <> show (select fst ps)
  putStrLn $ "Bob: " <> show (select snd ps)
  where
    select s = combine . map (first s)

combine :: Eq a => [(a, Double)] -> [(a, Double)]
combine ps = zip us $ map prob us
  where
    us = nub . map fst $ ps
    prob u = sum [ p | (e, p) <- ps, e == u]

parse :: [String] -> Char
parse [[c]]
  | c `elem` "01+-" = c
  | otherwise = error "Only 0, 1, + or - allowed"
parse _ = error "Expected a single 0, 1, + or - as argument"

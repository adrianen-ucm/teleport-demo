module Teleport (protocol, bell, teleport) where

import           Control.Monad ((>=>))
import           Quipper       (Circ, Qubit, controlled, controlled_not,
                                controlled_not_at, gate_X, gate_Z_at, hadamard,
                                hadamard_at, qc_init)

protocol :: Qubit -> Circ ((Qubit, Qubit), Qubit)
protocol a = do
  (b1, b2) <- qc_init (False, False) >>= bell
  teleport ((a, b1), b2)
 
bell :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
bell (x, y) = hadamard x >>= controlled_not y

teleport :: ((Qubit, Qubit), Qubit) -> Circ ((Qubit, Qubit), Qubit)
teleport ((a, b1), b2) = do
  controlled_not_at b1 a
  hadamard_at a
  correct_at b2
  pure ((a, b1), b2)
  where
    correct_at = (`controlled` b1) . gate_X
             >=> (`controlled` a ) . gate_Z_at

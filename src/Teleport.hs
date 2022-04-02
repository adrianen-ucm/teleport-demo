module Teleport (protocol, bell, teleport) where

import           Quipper (Circ, Qubit, controlled, controlled_not,
                          controlled_not_at, gate_X_at, gate_Z_at, hadamard,
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
  gate_X_at b2 `controlled` b1
  gate_Z_at b2 `controlled` a
  pure ((a, b1), b2)

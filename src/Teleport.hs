module Teleport (protocol, bell, teleport) where

import           Quipper (Bit, Circ, Qubit, controlled, controlled_not,
                          controlled_not_at, gate_X_at, gate_Z_at, hadamard,
                          hadamard_at, measure, qc_init)

-- | Example circuit of the protocol for teleporting a given qubit.
protocol :: Qubit -> Circ ((Bit, Bit), Qubit)
protocol a = do
  (b1, b2) <- qc_init (False, False) >>= bell
  teleport ((a, b1), b2)

-- | Circuit for building Bell states.
bell :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
bell (x, y) = hadamard x >>= controlled_not y

-- | Quantum state teleportation circuit. The first pair of qubits belong to
-- Alice and the last one to Bob.
teleport :: ((Qubit, Qubit), Qubit) -> Circ ((Bit, Bit), Qubit)
teleport ((a, b1), b2) = do
  -- Alice operates with her qubits
  controlled_not_at b1 a
  hadamard_at a
  -- She measures them and sends the results to Bob
  (m1, m2) <- measure (a, b1)
  -- Bob operates with his qubit according to the received information
  gate_X_at b2 `controlled` m2
  gate_Z_at b2 `controlled` m1
  -- Bob's qubit should now be in the state that the first one of Alice was
  pure ((m1, m2), b2)

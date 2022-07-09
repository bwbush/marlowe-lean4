

import M.Marlowe.Examples
import M.Marlowe.Language
import M.Marlowe.Proof
import M.Marlowe.Semantics


namespace Marlowe


open Marlowe.Examples


def main : IO UInt32 :=
  do
    IO.println (repr executeTrivial)
    IO.println checkTrivial
    return 0


end Marlowe

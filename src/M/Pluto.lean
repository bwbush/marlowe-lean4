

import M.Pluto.Examples
import M.Pluto.Language

/-  -- The Pluto proofs take about 90 minutes and 30 GB of memory to run.
import M.Pluto.Proof
-/


namespace Pluto


open Pluto.Examples


def main : IO UInt32 :=
  do
    IO.print hello
    pure 0


end Pluto

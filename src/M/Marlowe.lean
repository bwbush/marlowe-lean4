

import M.Marlowe.Language
import M.Marlowe.Semantics
import M.Marlowe.Semantics.Proof

import M.Marlowe.Intermediate.Examples
import M.Marlowe.Semantics.Examples


namespace Marlowe


open Marlowe.Intermediate.Examples
open Marlowe.Semantics.Examples


def main : IO UInt32 :=
  do
    IO.println (repr executeTrivial)
    IO.println ""
    IO.println (repr testIntermediate)
    IO.println ""
    IO.println checkTrivial
    return 0


end Marlowe

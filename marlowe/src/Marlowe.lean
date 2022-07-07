

import Marlowe.Examples


open Marlowe.Examples


def main : IO UInt32 :=
  do
    IO.println (repr executeTrivial)
    IO.println checkTrivial
    return 0

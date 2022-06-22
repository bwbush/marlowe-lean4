

import Marlowe.Examples
import Marlowe.Semantics.Types


open Marlowe.Examples
open Marlowe.Semantics.Contract
open Marlowe.Semantics.Input


#eval (Role "abc")

#eval (Int.ofNat 3 : Timeout) + (Int.ofNat 3 : ChosenNum)


def main : IO UInt32 :=
  do
    IO.println (repr closeContract)
    return 0


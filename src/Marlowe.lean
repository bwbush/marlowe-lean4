

import Marlowe.Examples
import Marlowe.Language
import Marlowe.Proof

open Marlowe.Examples
open Marlowe.Language.Contract
open Marlowe.Language.Input
open Marlowe.Proof (falsity)

#check falsity


#eval (Role "abc")

#eval (Int.ofNat 3 : Timeout) + (Int.ofNat 3 : ChosenNum)


def main : IO UInt32 :=
  do
    IO.println (repr closeContract)
    return 0




import M.Marlowe
import M.Pluto
import M.PlutusCore
import M.PlutusTx


def main : IO UInt32 :=
  do
    let _ <- Marlowe.main
    IO.println ""
    let _ <- PlutusCore.main
    IO.println "\n"
    Pluto.main


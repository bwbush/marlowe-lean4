
import M.Marlowe
import M.Pluto
import M.PlutusCore


def main : IO UInt32 :=
  do
    let _ <- Marlowe.main
    IO.println ""
    PlutusCore.main

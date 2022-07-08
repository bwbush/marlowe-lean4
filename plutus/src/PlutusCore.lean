
import PlutusCore.ByteString
import PlutusCore.Data

open PlutusCore (Data)


def main : IO UInt32 :=
  do
    IO.print $ Data.Map [(Data.I 2, Data.Constr 2 [Data.I 3]), (Data.I 4, Data.List [Data.Constr 3 [Data.I 7, Data.I 8]])]
    pure 0

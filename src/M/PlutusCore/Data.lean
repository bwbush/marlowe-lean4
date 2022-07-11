

import M.PlutusCore.ByteString


namespace PlutusCore


inductive Data where
  | Constr : Int → _root_.List Data → Data
  | Map    : _root_.List (Data × Data) → Data
  | List   : _root_.List Data → Data
  | I      : Int → Data
  | B      : ByteString → Data
deriving Repr


private def showWrapped (prefixTrue : String) (suffixTrue : String) (prefixFalse : String) (wrap : Bool) (x : String) : String :=
  if wrap
    then prefixTrue ++ x ++ suffixTrue
    else prefixFalse ++ x

private def mapWrapper'    := showWrapped "Map ["  "]" ","
private def listWrapper'   := showWrapped "List [" "]" ","

private def showData (wrap : Bool): Data → String
  | Data.Constr n []         => "Constr " ++ toString n ++ " []"
  | Data.Constr n (f :: [])  => "Constr " ++ toString n ++ " [" ++ showData true f ++ "]"
  | Data.Constr n (f :: fs)  => "Constr " ++ toString n ++ " [" ++ showData true f ++ showData false (Data.List fs)
  | Data.Map  []             => "Map []"
  | Data.Map  ((p, q) :: []) => mapWrapper'  wrap $ "(" ++ showData true p ++ "," ++ showData true q ++ ")"
  | Data.Map  ((p, q) :: ps) => mapWrapper'  wrap $ "(" ++ showData true p ++ "," ++ showData true q ++ ")" ++ showData false (Data.Map ps)
  | Data.List []             => "List []"
  | Data.List (x :: [])      => listWrapper' wrap $ showData true x
  | Data.List (x :: xs)      => listWrapper' wrap $ showData true x ++ showData false (Data.List xs)
  | Data.I    i              => "I " ++ toString i
  | Data.B    b              => "B " ++ toString b

instance : ToString Data where
  toString := showData true


end PlutusCore


import PlutusCore.ByteString


namespace PlutusCore


inductive Data where
  | Constr : Int → _root_.List Data → Data 
  | Map    : _root_.List (Data × Data) → Data 
  | List   : _root_.List Data → Data
  | I      : Int → Data
  | B      : ByteString → Data
deriving Repr

private def showData : Data → String
  | Data.Constr n fs => "Constr " ++ toString n ++ " [" ++ String.intercalate "," (fs.map showData) ++ "]"
  | Data.Map ps      => "Map  [" ++ String.intercalate "," (ps.map $ fun (k, v) => "(" ++ showData k ++ "," ++ showData v ++ ")") ++ "]" 
  | Data.List xs     => "List [" ++ String.intercalate "," (xs.map showData) ++ "]"
  | Data.I i         => "I " ++ toString i
  | Data.B b         => "B " ++ toString b

instance : ToString Data where
  toString := showData


end PlutusCore

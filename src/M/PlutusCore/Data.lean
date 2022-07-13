

import M.PlutusCore.ByteString


namespace PlutusCore


inductive Data where
  | Constr : Int → _root_.List Data → Data
  | Map    : _root_.List (Data × Data) → Data
  | List   : _root_.List Data → Data
  | I      : Int → Data
  | B      : ByteString → Data
deriving Repr


mutual

  private def intercalate' : _root_.List Data → String
    | []      => ""
    | [x]     => showData x
    | x :: xs => showData x ++ "," ++ intercalate' xs

  private def intercalate'' : _root_.List (Data × Data) → String
    | []            => ""
    | [(k, v)]      => "(" ++ showData k ++ "," ++ showData v ++ ")"
    | (k, v) :: kvs => "(" ++ showData k ++ "," ++ showData v ++ ")," ++ intercalate'' kvs

  private def showData : Data → String
    | Data.Constr n fs => "Constr " ++ toString n ++ " [" ++ intercalate' fs ++ "]"
    | Data.Map  kvs    => "Map [" ++ intercalate'' kvs ++ "]"
    | Data.List xs     => "List [" ++ intercalate' xs ++ "]"
    | Data.I    i      => "I " ++ toString i
    | Data.B    b      => "B " ++ toString b

end

instance : ToString Data where
  toString := showData


end PlutusCore

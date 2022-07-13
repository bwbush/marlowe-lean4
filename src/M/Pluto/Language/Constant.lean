

import M.PlutusCore


namespace Pluto.Language


open PlutusCore (ByteString Data)


universe u


inductive ConstantT (ann : Type u) where
  | I : ann → Int → ConstantT ann
  | S : ann → ByteString → ConstantT ann
  | T : ann → String → ConstantT ann
  | U : ann → ConstantT ann
  | B : ann → Bool → ConstantT ann
  | D : ann → Data → ConstantT ann
deriving Repr

export ConstantT (I S T U B D)

open ConstantT


mutual

  def intercalateData : _root_.List Data → String
    | []      => ""
    | [x]     => showData x
    | x :: xs => showData x ++ "," ++ intercalateData xs

  def intercalateData' : _root_.List (Data × Data) → String
    | []            => ""
    | [(k, v)]      => showData k ++ "=" ++ showData v
    | (k, v) :: kvs => showData k ++ "=" ++ showData v ++ "," ++ intercalateData' kvs

  def showData : Data → String
    | Data.Constr n fs => "sigma" ++ toString n ++ ".[" ++ intercalateData fs ++ "]"
    | Data.Map  kvs    => "{" ++ intercalateData' kvs ++ "}"
    | Data.List xs     => "[" ++ intercalateData xs ++ "]"
    | Data.I    i      => toString i
    | Data.B    b      => "0x" ++ toString b

end

instance : ToString (ConstantT ann) where
  toString : ConstantT ann → String
    | I _ i     => toString i
    | S _ s     => "0x" ++ toString s
    | T _ t     => "\"" ++ toString t ++ "\""
    | U _       => "()"
    | B _ true  => "True"
    | B _ false => "False"
    | D _ d     => "data " ++ showData d


end Pluto.Language

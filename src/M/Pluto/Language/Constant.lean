

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

private def showWrapped (prefixTrue : String) (suffixTrue : String) (prefixFalse : String) (wrap : Bool) (x : String) : String :=
  if wrap
    then prefixTrue ++ x ++ suffixTrue
    else prefixFalse ++ x

private def mapWrapper'    := showWrapped "{"  "}" ","
private def listWrapper'   := showWrapped "[" "]" ","

def showData (wrap : Bool): Data → String
  | Data.Constr n []         => "sigma" ++ toString n ++ ".[]"
  | Data.Constr n (f :: [])  => "sigma" ++ toString n ++ ".[" ++ showData true f ++ "]"
  | Data.Constr n (f :: fs)  => "sigma" ++ toString n ++ ".[" ++ showData true f ++ showData false (Data.List fs) ++ "]"
  | Data.Map  []             => "{}"
  | Data.Map  ((p, q) :: []) => mapWrapper'  wrap $ showData true p ++ "=" ++ showData true q
  | Data.Map  ((p, q) :: ps) => mapWrapper'  wrap $ showData true p ++ "=" ++ showData true q ++ showData false (Data.Map ps)
  | Data.List []             => "[]"
  | Data.List (x :: [])      => listWrapper' wrap $ showData true x
  | Data.List (x :: xs)      => listWrapper' wrap $ showData true x ++ showData false (Data.List xs)
  | Data.I    i              => toString i
  | Data.B    b              => "0x" ++ toString b

instance : ToString (ConstantT ann) where
  toString : ConstantT ann → String
    | I _ i     => toString i
    | S _ s     => "0x" ++ toString s
    | T _ t     => "\"" ++ toString t ++ "\""
    | U _       => "()"
    | B _ true  => "True"
    | B _ false => "False"
    | D _ d     => "data " ++ showData true d

example : toString (I () 3                                           ) = "3"        := rfl
example : toString (S () $ ByteString.mk $ ByteArray.mk #[0xca, 0xfe]) = "0xcafe"   := rfl
example : toString (T () "test"                                      ) = "\"test\"" := rfl
example : toString (U ()                                             ) = "()"       := rfl
example : toString (B () true                                        ) = "True"     := rfl
example : toString (B () false                                       ) = "False"    := rfl

private def testData :=
  Data.List
    [
      Data.Map []
    , Data.Map [
        (Data.I 3, Data.List [])
      ]
    , Data.Map [
        (Data.B $ ByteString.mk $ ByteArray.mk #[0xca, 0xfe], Data.List [Data.I 2])
      ]
    , Data.Map [
        (Data.I 10, Data.I 20)
      , (Data.I 30, Data.I 40)
    ]
    , Data.Constr 2 [Data.I 9, Data.I (-10)]
    , Data.Constr 1 [Data.I (-11)]
    , Data.Constr 0 []
    ]

#eval toString (D () testData) == "data [{},{3=[]},{0xcafe=[2]},{10=20,30=40},sigma2.[9,-10],sigma1.[-11],sigma0.[]]"


end Pluto.Language

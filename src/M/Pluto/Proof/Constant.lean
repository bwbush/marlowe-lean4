

import M.Pluto.Language.Constant
import M.PlutusCore


namespace Pluto.Proof


open Pluto.Language
open PlutusCore (ByteString Data)


theorem show_constant_integer (i : Int) : toString (I ann i) = toString i :=
  rfl


theorem show_constant_bytestring (s : ByteString) : toString (S ann s) = "0x" ++ toString s :=
  rfl

  
theorem show_constant_text (t : String) : toString (T ann t) = "\"" ++ toString t ++ "\"" :=
  rfl

  
theorem show_constant_unit : toString (U ann ) = "()" :=
  rfl

  
theorem show_constant_true : toString (B ann true) = "True" :=
  rfl

  
theorem show_constant_false : toString (B ann false) = "False" :=
  rfl

theorem show_constant_data_integer : toString (D ann (Data.I i)) = "data " ++ toString i :=
  by simp [toString, showData]


theorem show_constant_data_bytestring : toString (D ann (Data.B b)) = "data " ++ ("0x" ++ toString b) :=
  by simp [toString, showData]

  
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

example : toString (D () testData) = "data [{},{3=[]},{0xcafe=[2]},{10=20,30=40},sigma2.[9,-10],sigma1.[-11],sigma0.[]]" :=
  by simp [toString, testData, showData, intercalateData, intercalateData']


end Pluto.Proof



import M.Pluto.Language.AST


open Pluto.Language
open PlutusCore (ByteString Data)


private def toString' (term : Term ann) : String :=
  if hasSpace term
    then "(" ++ toString term ++ ")"
    else toString term


attribute [local simp] toString toString' showTerm showTerm' showBinding showBindings showData intercalateData intercalateData'


@[simp] theorem show_name : toString (Name.mk name) = name :=
  by rfl


@[simp] theorem show_ast_var : toString (Var ann name) = toString name :=
  by simp


@[simp] theorem show_ast_lambda : toString (Lambda ann names term) = "(\\" ++ intercalateName names ++ " -> " ++ toString term ++ ")" :=
  by simp


@[simp] theorem show_ast_apply : toString (Apply ann term term') = toString' term ++ " " ++ toString' term' :=
  by simp


@[simp] theorem show_ast_force : toString (Force ann term) = "! " ++ toString' term :=
  by simp


@[simp] theorem show_ast_delay : toString (Delay ann term) = "# " ++ toString' term :=
  by simp


@[simp] theorem show_ast_constant : toString (Constant ann cons) = toString cons :=
  by simp


@[simp] theorem show_ast_builtin : toString (Builtin ann builtin) = toString builtin :=
  by simp


@[simp] theorem show_ast_let : toString (Let ann bindings term) = "let " ++ showBindings bindings ++ " in " ++ toString term :=
  by simp


@[simp] theorem show_ast_if : toString (IfThenElse ann term term' term'') = "if " ++ toString term ++ " then " ++ toString term' ++ " else " ++ toString term'' :=
  by simp


private def test01 : Term Unit :=
  Constant () $ I () 1

example : toString test01 = "1" :=
  by rfl


private def test02 : Term Unit :=
  let x := Name.mk "x"
  Lambda () [x]
    $ Var () x

example : toString test02 = "(\\x -> x)" :=
  by simp [test02]


private def test03 : Term Unit :=
  Let ()
    [Binding () (Name.mk "x") $ Constant () $ I () 1]
    $ Constant () (I () 42)

example : toString test03 = "let x = 1 in 42" :=
  by simp [test03]


private def test04 : Term Unit :=
  Let ()
    [Binding () (Name.mk "id") test02]
    $ Constant () (I () 42)

example : toString test04 = "let id = (\\x -> x) in 42" :=
  by simp [test04, test02]


private def test05 : Term Unit :=
  let f := Name.mk "f"
  Let ()
    [Binding () f test02]
    $ Apply ()
      (Var () f)
      $ Constant () (I () 1)

example : toString test05 = "let f = (\\x -> x) in f 1" :=
  by simp [test05, test02]


private def test06 : Term Unit :=
  let f := Name.mk "f"
  let res := Name.mk "res"
  Let ()
    [
      Binding () f
        test02
    , Binding () res
        $ Delay ()
        $ Apply ()
          (Var () f)
          $ Constant () (I () 1)
    ]
    $ Var () res

example : toString test06 = "let f = (\\x -> x); res = # (f 1) in res" :=
  by simp [test06, test02]


private def test07 : Term Unit :=
  let f := Name.mk "f"
  let res := Name.mk "res"
  Let ()
    [
      Binding () f
        test02
    , Binding () res
        $ Delay ()
        $ Apply ()
          (Var () f)
          $ Constant () (I () 1)
      ]
    $ Force ()
    $ Var () res

example : toString test07 = "let f = (\\x -> x); res = # (f 1) in ! res" :=
  by simp [test07, test02]


private def test08 : Term Unit :=
  let iff := Name.mk "iff"
  let cond := Name.mk "cond"
  let x := Name.mk "x"
  let y := Name.mk "y"
  Let ()
    [
      Binding () iff
        $ Lambda () [cond, x, y]
        $ IfThenElse ()
            (Var () cond)
            (Var () x)
            (Var () y)
    ]
    $ Force ()
    $ Apply ()
      (
        Apply ()
          (Var () iff)
          $ Delay ()
          $ Constant () (I () 42)
      )
      $ Delay ()
      $ Constant () (I () 7)

example : toString test08 = "let iff = (\\cond x y -> if cond then x else y) in ! ((iff (# 42)) (# 7))" :=
  by simp [test08, test02]


example : toString (Constant () $ B () true) = "True" := rfl


example : toString (Constant () $ B () false) = "False" := rfl


example : toString (Constant () $ I () 42) = "42" := rfl


example : toString (Constant () $ I () (-2)) = "-2" := rfl


example : toString (Constant () $ S () $ ByteString.mk $ ByteArray.mk #[0x41]) = "0x41" := rfl


example : toString (Constant () $ T () "foobar") = "\"foobar\"" := rfl


example : toString (Constant () $ U ()) = "()" := rfl


example : toString (Constant () $ D () $ Data.I 42) = "data 42" := rfl


example : toString (Constant () $ D () $ Data.B $ ByteString.mk $ ByteArray.mk #[0x41]) = "data 0x41" := rfl


example : toString (Constant () $ D () $ Data.List [Data.I 1, Data.I 2, Data.I 3]) = "data [1,2,3]" :=
  by simp


example : toString (Constant () $ D () $ Data.List [Data.I 1, Data.B $ ByteString.mk $ ByteArray.mk #[0x41], Data.List [Data.I 42], Data.Map [(Data.B $ ByteString.mk $ ByteArray.mk #[0xfe], Data.I 7)]]) = "data [1,0x41,[42],{0xfe=7}]" :=
  by simp


example : toString (Constant () $ D () $ Data.Map [(Data.I 1, Data.B $ ByteString.mk $ ByteArray.mk #[0x42]), (Data.B $ ByteString.mk $ ByteArray.mk #[0xfe], Data.I 42),(Data.List [Data.I 1, Data.I 2], Data.List [Data.I 3, Data.I 4]),(Data.Map [(Data.I 1, Data.I 3)], Data.I 4)]) = "data {1=0x42,0xfe=42,[1,2]=[3,4],{1=3}=4}" :=
  by simp


example : toString (Constant () $ D () $ Data.Constr 0 [Data.I 1, Data.B $ ByteString.mk $ ByteArray.mk #[0x41]]) = "data sigma0.[1,0x41]" :=
  by simp


example : toString (Program.mk $ Lambda () [Name.mk "x"] (Constant () $ I () 3)) = "(\\x -> 3)" :=
  by simp

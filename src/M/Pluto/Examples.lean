

import M.Pluto.Language


namespace Pluto.Examples


open Pluto.Language


def echo : Term Unit :=
  let x := Name.mk "x"
  Lambda ()
    [x]
    $ Var () x

#eval toString echo == "(\\x -> x)"


def hello : Term Unit :=
  let trace := Name.mk "trace"
  let s := Name.mk "s"
  let x := Name.mk "x"
  let defaultGreeting := Name.mk "defaultGreeting"
  let greet := Name.mk "greet"
  let greeting := Name.mk "greeting"
  let name := Name.mk "name"
  let nameData := Name.mk "nameData"
  Let ()
    [
      Binding () trace
        $ Lambda () [s, x]
        $ Apply ()
          (
            Apply ()
              (
                Force ()
                  $ Builtin () Trace
              )
              $ Var () s
          )
          $ Var () x
    , Binding () defaultGreeting
        $ Constant () (T () "Hello")
    , Binding () greet
        $ Lambda () [greeting, name]
        $ Apply ()
          (
            Apply ()
              (Builtin () AppendString)
              $ Apply ()
                  (
                    Apply ()
                      (Builtin () AppendString)
                      (Var () greeting)
                  )
                  $ Constant () (T () ", ")
          )
        $ Apply ()
            (
              Apply ()
                (Var () trace)
                (
                  Apply ()
                    (
                      Apply ()
                        (Builtin () AppendString)
                        $ Constant () (T () "Name is: ")
                    )
                    $ Var () name
                )
            )
            $ Var () name
    ]
    $ Lambda () [nameData]
    $ Apply ()
      (
        Apply ()
          (Var () greet)
          (Var () defaultGreeting)
      )
      $ Apply ()
        (Builtin () DecodeUtf8)
        $
          Apply ()
            (Builtin () UnBData)
            $ Var () nameData

-- Test with `pluto run test.pluto 0x576f726c64`
#eval toString hello == "let trace = (\\s x -> ((! Trace) s) x); defaultGreeting = \"Hello\"; greet = (\\greeting name -> (AppendString ((AppendString greeting) (\", \"))) ((trace ((AppendString (\"Name is: \")) name)) name)) in (\\nameData -> (greet defaultGreeting) (DecodeUtf8 (UnBData nameData)))"


end Pluto.Examples

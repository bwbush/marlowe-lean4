

import Marlowe.Semantics


namespace Marlowe.Proof


open Marlowe.Semantics (divide)


example : divide    0    0  =  0 := rfl
example : divide    0    6  =  0 := rfl
example : divide   12    0  =  0 := rfl
example : divide   12    6  =  2 := rfl
example : divide   13    6  =  2 := rfl
example : divide   14    6  =  2 := rfl
example : divide   15    6  =  2 := rfl
example : divide   16    6  =  3 := rfl
example : divide   17    6  =  3 := rfl
example : divide   18    6  =  3 := rfl
example : divide   19    6  =  3 := rfl
example : divide   20    6  =  3 := rfl
example : divide   21    6  =  4 := rfl
example : divide   22    6  =  4 := rfl
example : divide   23    6  =  4 := rfl
example : divide   24    6  =  4 := rfl
example : divide    0  (-6) =  0 := rfl
example : divide (-12)   0  =  0 := rfl
example : divide   12  (-6) = -2 := rfl
example : divide (-13)   6  = -2 := rfl
example : divide   14  (-6) = -2 := rfl
example : divide (-15)   6  = -2 := rfl
example : divide   16  (-6) = -3 := rfl
example : divide (-17)   6  = -3 := rfl
example : divide   18  (-6) = -3 := rfl
example : divide (-19)   6  = -3 := rfl
example : divide   20  (-6) = -3 := rfl
example : divide (-21)   6  = -4 := rfl
example : divide   22  (-6) = -4 := rfl
example : divide (-23)   6  = -4 := rfl
example : divide   24  (-6) = -4 := rfl
example : divide    9    1  =  9 := rfl
example : divide ( -9)   1  = -9 := rfl


theorem divide_by_one (x : Int) : divide x 1 = x :=
  match x with
    | 0 => by rfl
    | _ => sorry


end Marlowe.Proof

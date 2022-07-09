

import M.Marlowe.Language.Contract
import M.PlutusCore


namespace Marlowe.Language.Input


open Marlowe.Language.Contract
open PlutusCore (ByteString)


def ChosenNum := Int

def ChosenNum.toInt : ChosenNum → Int :=
  fun (x : Int) => x

deriving instance BEq, Inhabited, LE, LT, Ord, Repr for ChosenNum

def ChosenNum.decLt (x y : ChosenNum) : Decidable (x < y) :=
  inferInstanceAs (Decidable (x.toInt < y.toInt))

instance (x y : ChosenNum) : Decidable (x < y) := ChosenNum.decLt x y

def ChosenNum.decLe (x y : ChosenNum) : Decidable (x <= y) :=
  inferInstanceAs (Decidable (x.toInt <= y.toInt))

instance (x y : ChosenNum) : Decidable (x <= y) := ChosenNum.decLe x y


inductive InputContent where
  | IDeposit : AccountId → PartyT → TokenT → Int → InputContent
  | IChoice : ChoiceIdT → ChosenNum → InputContent
  | INotify : InputContent
deriving BEq, Repr

export InputContent (IDeposit IChoice INotify)


inductive Input where
  | NormalInput     : InputContent → Input
  | MerkleizedInput : InputContent → ByteString → Contract → Input
deriving BEq, Repr

export Input (NormalInput MerkleizedInput)


end Marlowe.Language.Input

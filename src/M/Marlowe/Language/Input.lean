

import M.Marlowe.Language.Contract
import M.Marlowe.Primitives


namespace Marlowe.Language.Input


open Marlowe.Language.Contract
open Marlowe.Primitives (ByteString Integer)


def ChosenNum := Integer

deriving instance BEq, Inhabited, Repr for ChosenNum


inductive InputContent where
  | IDeposit : AccountId → PartyT → TokenT → Integer → InputContent
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

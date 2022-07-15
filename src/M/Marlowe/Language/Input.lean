

import M.Marlowe.Language.Contract


namespace Marlowe.Language.Input


open Marlowe.Language.Contract


structure ChosenNum (ι : Type) where
  val : ι
deriving Inhabited, Repr


inductive InputContent (β ι : Type) where
  | IDeposit : AccountId  β → PartyT β → TokenT  β → ι → InputContent β ι
  | IChoice  : ChoiceIdT  β → ChosenNum  ι → InputContent β ι
  | INotify  : InputContent β ι
deriving Repr

export InputContent (IDeposit IChoice INotify)


inductive Input (β ι τ : Type) where
  | NormalInput     : InputContent β ι → Input β ι τ
  | MerkleizedInput : InputContent β ι → β → Contract β ι τ → Input β ι τ
deriving Repr

export Input (NormalInput MerkleizedInput)


end Marlowe.Language.Input

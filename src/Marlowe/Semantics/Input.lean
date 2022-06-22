

import Marlowe.Semantics.Contract
import Marlowe.Semantics.Primitives


namespace Marlowe.Semantics.Input


open Marlowe.Semantics.Contract
open Marlowe.Semantics.Primitives


def ChosenNum := Integer

deriving instance Ord, Repr for ChosenNum


inductive InputContent where
| IDeposit : AccountId → PartyT → TokenT → Integer → InputContent
| IChoice : ChoiceIdT → ChosenNum → InputContent
| INotify : InputContent
deriving Repr

export InputContent (IDeposit IChoice INotify)


end Marlowe.Semantics.Input

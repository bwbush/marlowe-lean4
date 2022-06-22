

import Marlowe.Language.Contract
import Marlowe.Primitives


namespace Marlowe.Language.Input


open Marlowe.Language.Contract
open Marlowe.Primitives


def ChosenNum := Integer

deriving instance Ord, Repr for ChosenNum


inductive InputContent where
| IDeposit : AccountId → PartyT → TokenT → Integer → InputContent
| IChoice : ChoiceIdT → ChosenNum → InputContent
| INotify : InputContent
deriving Repr

export InputContent (IDeposit IChoice INotify)


end Marlowe.Language.Input



import M.Plutus
import M.PlutusCore


namespace Marlowe.Language.Contract


open Plutus.V1.Ledger.Time (POSIXTime)
open PlutusCore (ByteString)


def Timeout := POSIXTime

deriving instance BEq, Inhabited, Ord, Repr for Timeout


def CurrencySymbol := ByteString

deriving instance BEq, Inhabited, Ord, Repr for CurrencySymbol


def TokenName := ByteString

deriving instance BEq, Inhabited, Ord, Repr for TokenName


inductive TokenT where
  | Ada   : TokenT
  | Token : CurrencySymbol → TokenName → TokenT
deriving BEq, Ord, Repr

instance : Inhabited TokenT where
  default := TokenT.Ada

export TokenT (Ada Token)


inductive PartyT where
  | PubKey : ByteString → PartyT
  | Role   : TokenName  → PartyT
deriving BEq, Ord, Repr

export PartyT (PubKey Role)


def AccountId := PartyT

deriving instance BEq, Ord, Repr for AccountId


inductive Payee where
  | Account : AccountId → Payee
  | Party   : PartyT → Payee
deriving BEq, Repr

export Payee (Account Party)


inductive BoundT where
  | Bound : Int → Int → BoundT
deriving BEq, Repr

export BoundT (Bound)


inductive ChoiceIdT where
  | ChoiceId : ByteString → PartyT → ChoiceIdT
deriving BEq, Ord, Repr

export ChoiceIdT (ChoiceId)


inductive ValueIdT where
  | ValueId : ByteString → ValueIdT
deriving BEq, Ord, Repr

export ValueIdT (ValueId)


mutual

  inductive Value where
    | AvailableMoney    : AccountId → TokenT → Value
    | Constant          : Int → Value
    | NegValue          : Value → Value
    | AddValue          : Value → Value → Value
    | SubValue          : Value → Value → Value
    | MulValue          : Value → Value → Value
    | DivValue          : Value → Value → Value
    | Scale             : Int → Int → Value → Value
    | ChoiceValue       : ChoiceIdT → Value
    | TimeIntervalStart : Value
    | TimeIntervalEnd   : Value
    | UseValue          : ValueIdT → Value
    | Cond              : Observation → Value → Value → Value
  deriving BEq, Repr

  inductive Observation where
    | AndObs         : Observation → Observation → Observation
    | OrObs          : Observation → Observation → Observation
    | NotObs         : Observation → Observation
    | ChoseSomething : ChoiceIdT → Observation
    | ValueGE        : Value → Value → Observation
    | ValueGT        : Value → Value → Observation
    | ValueLT        : Value → Value → Observation
    | ValueLE        : Value → Value → Observation
    | ValueEQ        : Value → Value → Observation
    | TrueObs        : Observation
    | FalseObs       : Observation
  deriving BEq, Repr

end

export Value (AvailableMoney Constant NegValue AddValue SubValue MulValue DivValue Scale ChoiceValue TimeIntervalStart TimeIntervalEnd UseValue Cond)

export Observation (AndObs OrObs NotObs ChoseSomething ValueGE ValueGT ValueLT ValueLE ValueEQ TrueObs FalseObs)


inductive Action where
  | Deposit : AccountId → PartyT → TokenT → Value → Action
  | Choice  : ChoiceIdT → List BoundT → Action
  | Notify  : Observation → Action
 deriving BEq, Repr

 export Action (Deposit Choice Notify)


mutual

  inductive CaseT where
    | Case           : Action → Contract → CaseT
    | MerkleizedCase : Action → ByteString → CaseT
  deriving BEq, Repr

  inductive Contract where
    | Close  : Contract
    | Pay    : AccountId → Payee → TokenT → Value → Contract → Contract
    | If     : Observation → Contract → Contract → Contract
    | When   : List CaseT → Timeout → Contract → Contract
    | Let    : ValueIdT → Value → Contract → Contract
    | Assert : Observation → Contract → Contract
  deriving BEq, Repr

end

  instance : Inhabited Contract where
    default := Contract.Close

export CaseT (Case MerkleizedCase)

export Contract (Close Pay If When Let Assert)


end Marlowe.Language.Contract

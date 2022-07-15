

import M.Marlowe.Language.Class


namespace Marlowe.Language.Contract


open Marlowe.Language.Class


structure Timeout (τ : Type) where
  val : τ
deriving Inhabited, Repr


structure CurrencySymbol (β : Type) where
  val : β
deriving Repr

instance [AEq β ω] : AEq (CurrencySymbol β) ω where
  aeq x y := x.val A== y.val


structure TokenName (β : Type) where
  val : β
deriving Repr

instance [AEq β ω] : AEq (TokenName β) ω where
  aeq x y := x.val A== y.val


inductive TokenT (β : Type) where
  | Ada   : TokenT β
  | Token : CurrencySymbol β → TokenName β → TokenT β
deriving Repr

instance [AEq β ω] [ABool ω] : AEq (TokenT β) ω where
  aeq : TokenT β → TokenT β → ω
    | TokenT.Ada        , TokenT.Ada           => atrue
    | TokenT.Token cs tn, TokenT.Token cs' tn' => cs A== cs' A&& tn A== tn'
    | _                 , _                    => afalse

instance : Inhabited (TokenT β) where
  default := TokenT.Ada

export TokenT (Ada Token)


inductive PartyT (β : Type) where
  | PubKey : β → PartyT β
  | Role   : TokenName β → PartyT β
deriving Repr

instance [AEq β ω] [ABool ω]: AEq (PartyT β) ω where
  aeq : PartyT β → PartyT β → ω
    | PartyT.PubKey x, PartyT.PubKey y => x A== y
    | PartyT.Role   x, PartyT.Role   y => x A== y
    | _              , _               => afalse


export PartyT (PubKey Role)


structure AccountId (β : Type) where
  val : PartyT β
deriving Repr

instance [AEq (PartyT β) ω] : AEq (AccountId β) ω where
  aeq x y := x.val A== y.val


inductive Payee (β : Type) where
  | Account : AccountId β → Payee β
  | Party   : PartyT β → Payee β
deriving Repr

export Payee (Account Party)


inductive BoundT (ι : Type) where
  | Bound : ι → ι → BoundT ι
deriving Repr

export BoundT (Bound)


inductive ChoiceIdT (β : Type) where
  | ChoiceId : β → PartyT β → ChoiceIdT β
deriving Repr

instance [AEq β ω] [ABool ω]: AEq (ChoiceIdT β) ω where
  aeq : ChoiceIdT β → ChoiceIdT β → ω
    | ChoiceIdT.ChoiceId c p, ChoiceIdT.ChoiceId c' p' => c A== c' A&& p A== p'

export ChoiceIdT (ChoiceId)


inductive ValueIdT (β : Type) where
  | ValueId : β → ValueIdT β
deriving Repr

instance [AEq β ω] : AEq (ValueIdT β) ω where
  aeq : ValueIdT β → ValueIdT β → ω
    | ValueIdT.ValueId x, ValueIdT.ValueId y => x A== y

export ValueIdT (ValueId)


mutual

  inductive Value (β ι : Type) where
    | AvailableMoney    : AccountId β → TokenT β → Value β ι
    | Constant          : ι → Value β ι
    | NegValue          : Value β ι → Value β ι
    | AddValue          : Value β ι → Value β ι → Value β ι
    | SubValue          : Value β ι → Value β ι → Value β ι
    | MulValue          : Value β ι → Value β ι → Value β ι
    | DivValue          : Value β ι → Value β ι → Value β ι
    | ChoiceValue       : ChoiceIdT β → Value β ι
    | TimeIntervalStart : Value β ι
    | TimeIntervalEnd   : Value β ι
    | UseValue          : ValueIdT β → Value β ι
    | Cond              : Observation β ι → Value β ι → Value β ι → Value β ι
  deriving Repr

  inductive Observation (β ι : Type) where
    | AndObs         : Observation β ι → Observation β ι → Observation β ι
    | OrObs          : Observation β ι → Observation β ι → Observation β ι
    | NotObs         : Observation β ι → Observation β ι
    | ChoseSomething : ChoiceIdT β → Observation β ι
    | ValueGE        : Value β ι → Value β ι → Observation β ι
    | ValueGT        : Value β ι → Value β ι → Observation β ι
    | ValueLT        : Value β ι → Value β ι → Observation β ι
    | ValueLE        : Value β ι → Value β ι → Observation β ι
    | ValueEQ        : Value β ι → Value β ι → Observation β ι
    | TrueObs        : Observation β ι
    | FalseObs       : Observation β ι
  deriving Repr

end

export Value (AvailableMoney Constant NegValue AddValue SubValue MulValue DivValue ChoiceValue TimeIntervalStart TimeIntervalEnd UseValue Cond)

export Observation (AndObs OrObs NotObs ChoseSomething ValueGE ValueGT ValueLT ValueLE ValueEQ TrueObs FalseObs)


inductive Action (β ι : Type) where
  | Deposit : AccountId β → PartyT β → TokenT β → Value β ι → Action β ι
  | Choice  : ChoiceIdT β → List (BoundT ι) → Action β ι
  | Notify  : Observation β ι → Action β ι
 deriving Repr

 export Action (Deposit Choice Notify)


mutual

  inductive CaseT (β ι τ : Type) where
    | Case           : Action β ι → Contract β ι τ → CaseT β ι τ
    | MerkleizedCase : Action β ι → β → CaseT β ι τ
  deriving Repr

  inductive Contract (β ι τ : Type) where
    | Close  : Contract β ι τ
    | Pay    : AccountId β → Payee β → TokenT β → Value β ι → Contract β ι τ → Contract β ι τ
    | If     : Observation β ι → Contract β ι τ → Contract β ι τ → Contract β ι τ
    | When   : List (CaseT β ι τ) → Timeout τ → Contract β ι τ → Contract β ι τ
    | Let    : ValueIdT β → Value β ι → Contract β ι τ → Contract β ι τ
    | Assert : Observation β ι → Contract β ι τ → Contract β ι τ
  deriving Repr

end

instance : Inhabited (Contract β ι τ) where
  default := Contract.Close

export CaseT (Case MerkleizedCase)

export Contract (Close Pay If When Let Assert)


end Marlowe.Language.Contract

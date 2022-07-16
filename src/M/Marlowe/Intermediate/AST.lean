

import M.Marlowe.Language
import M.PlutusCore


namespace Marlowe.Intermediate


open Marlowe.Language.Contract
open Marlowe.Language.Input
open PlutusCore (ByteString)


inductive Bytes₂ where
  | ConstantBytes : ByteString → Bytes₂
  deriving Repr

export Bytes₂ (ConstantBytes)


mutual

  inductive Int₁ where
    | ConstantInt₁ : Int → Int₁
    | NegInt       : Int₁ → Int₁
    | AddInt       : Int₁ → Int₁ → Int₁
    | SubInt       : Int₁ → Int₁ → Int₁
    | MulInt       : Int₁ → Int₁ → Int₁
    | DivInt       : Int₁ → Int₁ → Int₁
    | Branch       : Bool₁ → Int₁ → Int₁ → Int₁
    | GetMoney     : AccountId → TokenT → Int₁
    | GetChoice    : ChoiceIdT → Int₁
    | GetValue     : ValueIdT → Int₁
    | GetStart     : Int₁
    | GetEnd       : Int₁
    | GetMin       : Int₁
    deriving Repr

  inductive Bool₁ where
    | ConstantBool₁ : Bool → Bool₁
    | AndBool       : Bool₁ → Bool₁ → Bool₁
    | OrBool        : Bool₁ → Bool₁ → Bool₁
    | NotBool       : Bool₁ → Bool₁
    | LtInt         : Int₁ → Int₁ → Bool₁
    | LeInt         : Int₁ → Int₁ → Bool₁
    | EqInt         : Int₁ → Int₁ → Bool₁
    | HasChosen     : ChoiceIdT → Bool₁
    deriving Repr

end

export Int₁ (ConstantInt₁ Branch GetMoney GetChoice GetValue GetStart GetEnd GetMin)

export Bool₁ (ConstantBool₁ HasChosen)


inductive Int₂ where
  | ConstantInt₂ : Int → Int₂
    | NegInt     : Int₂ → Int₂
    | AddInt     : Int₂ → Int₂ → Int₂
    | SubInt     : Int₂ → Int₂ → Int₂
    | MulInt     : Int₂ → Int₂ → Int₂
    | DivInt     : Int₂ → Int₂ → Int₂
  | Evaluate     : Int₁ → Int₂
  deriving Repr

export Int₂ (ConstantInt₂ Evaluate)


inductive Bool₂ where
  | ConstantBool₂ : Bool → Bool₂
  | EqBytes       : Bytes₂ → Bytes₂ → Bool₂
  | AndBool       : Bool₂ → Bool₂ → Bool₂
  | OrBool        : Bool₂ → Bool₂ → Bool₂
  | NotBool       : Bool₂ → Bool₂
  | InRange       : Int₂ × Int₂ → Int₂ → Bool₂
  | LtInt         : Int₂ → Int₂ → Bool₂
  | LeInt         : Int₂ → Int₂ → Bool₂
  | EqInt         : Int₂ → Int₂ → Bool₂
  | EqAccount     : AccountId → AccountId → Bool₂
  | EqParty       : PartyT → PartyT → Bool₂
  | EqToken       : TokenT → TokenT → Bool₂
  | EqChoice      : ChoiceIdT → ChoiceIdT → Bool₂
  | Observe       : Bool₁ → Bool₂
  deriving Repr

export Bool₂ (ConstantBool₂ InRange Observe)


inductive Op where
  -- TODO: Add types for induction on `List`, `InputContent`, `Action`, `CaseT`, etc.
  | SetMoney       : AccountId → TokenT → Int₂ → Op → Op
  | SetValue       : ValueIdT → Int₂ → Op → Op
  | SetChoice      : ChoiceIdT → Int₂ → Op → Op
  | SetMin         : Int₂ → Op → Op
  | MakePayment    : AccountId → Payee → TokenT → Int₂ → Op → Op
  | InputApplied   : Input → Op → Op
  | NewContract    : Contract → Op → Op
  | IfThenElse     : Bool₂ → Op → Op → Op
  | Guard          : Bool₂ → Op → Op → Op
  | Ensure         : Bool₂ → String → Op → Op
  | Trace          : String → Op → Op
  | TraceIf        : Bool₂ → String → Op → Op
  | Warn           : String → Op → Op
  | Fail           : String → Op
  | Done           : Contract → Op  -- NB: Includes payments to all accounts.
  | Wait           : Op
  | RemainingInput : List Input → Op → Op  -- FIXME: Remove unless `step` is used.
  deriving Repr

export Op (SetMoney SetValue SetChoice SetMin MakePayment InputApplied NewContract IfThenElse Guard Ensure Trace TraceIf Warn Fail Done Wait RemainingInput)


notation:10 o " ?₁ " x " :₁ " y => Int₁.Branch o x y
notation:10 o " ?ₒ " x " :ₒ " y => Op.IfThenElse o x y

infixl:35 " &&₁ " => Bool₁.AndBool
infixl:35 " &&₂ " => Bool₂.AndBool
infixl:30 " ||₁ " => Bool₁.OrBool
infixl:30 " ||₂ " => Bool₂.OrBool
infix:50  " <=₁ " => Bool₁.LeInt
infix:50  " <=₂ " => Bool₂.LeInt
infix:50  " ≤₁ "  => Bool₁.LeInt
infix:50  " ≤₂ "  => Bool₂.LeInt
infix:50  " <₁ "  => Bool₁.LtInt
infix:50  " <₂ "  => Bool₂.LtInt
infix:50  " >=₁ " => (flip Bool₁.LeInt)
infix:50  " >=₂ " => (flip Bool₂.LeInt)
infix:50  " ≥₁ "  => (flip Bool₁.LeInt)
infix:50  " ≥₂ "  => (flip Bool₂.LeInt)
infix:50  " >₁ "  => (flip Bool₁.LtInt)
infix:50  " >₂ "  => (flip Bool₂.LtInt)
infix:50  " ==₁ " => Bool₁.EqInt
infix:50  " ==₂ " => Bool₂.EqInt
infix:50  " ==ₐ " => Bool₂.EqAccount
infix:50  " ==ₚ " => Bool₂.EqParty
infix:50  " ==ₜ " => Bool₂.EqToken
infix:50  " ==ₖ " => Bool₂.EqChoice
infix:50  " ==ₓ " => Bool₂.EqBytes
infixl:65 " +₁ "  => Int₁.AddInt
infixl:65 " +₂ "  => Int₂.AddInt
infixl:65 " -₁ "  => Int₁.SubInt
infixl:65 " -₂ "  => Int₂.SubInt
infixl:70 " *₁ "  => Int₁.MulInt
infixl:70 " /₁ "  => Int₁.DivInt
infixl:70 " /₂ "  => Int₁.DivInt
prefix:100 "-₁"   => Int₁.NegInt
prefix:100 "-₂"   => Int₂.NegInt
prefix:100 "!₁"   => Bool₁.NotBool
prefix:100 "!₂"   => Bool₂.NotBool


end Marlowe.Intermediate

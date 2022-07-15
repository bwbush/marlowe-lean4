

import M.Marlowe.Language.Class
import M.Marlowe.Language.Contract
import M.Marlowe.Language.Input


namespace Marlowe.Language.State


open Marlowe.Language.Class
open Marlowe.Language.Contract
open Marlowe.Language.Input


structure Accounts (β ι : Type) (μ : Type → Type → Type) where
  val : μ (AccountId β × TokenT β) ι

instance [Repr (μ (AccountId β × TokenT β) ι)] : Repr (Accounts β ι μ) where
  reprPrec m prec := Repr.addAppParen ("Marlowe.Language.State.Accounts.mk " ++ repr m.val) prec


instance [AEq (AccountId β) ω] [AEq (TokenT β) ω] [ABool ω] : AEq (AccountId β × TokenT β) ω where
  aeq : AccountId β × TokenT β → AccountId β × TokenT β → ω
      | (a, t), (a', t') => a A== a' A&& t A== t'


structure State (β ι τ : Type) (μ : Type → Type → Type) where
  accounts    : Accounts β ι μ 
  choices     : μ (ChoiceIdT β) (ChosenNum ι)
  boundValues : μ (ValueIdT β) ι
  minTime     : τ

instance [Repr (Accounts β ι μ)] [Repr (μ (ChoiceIdT β) (ChosenNum ι))] [Repr (μ (ValueIdT β) ι)] [Repr τ] : Repr (State β ι τ μ) where
  reprPrec x prec := Repr.addAppParen ("Marlowe.Language.State.State.mk " ++ repr x.accounts ++ " " ++ repr x.choices ++ " " ++ repr x.boundValues ++ " " ++ repr x.minTime) prec

instance [BEq (Accounts β ι μ)] [BEq (μ (ChoiceIdT β) (ChosenNum ι))] [BEq (μ (ValueIdT β) ι)] [BEq τ] : BEq (State β ι τ μ) where
  beq x y := x.accounts == y.accounts
               && x.choices == y.choices
               && x.boundValues == y.boundValues
               && x.minTime == y.minTime

instance [Inhabited (Accounts β ι μ)] [Inhabited (μ (ChoiceIdT β) (ChosenNum ι))] [Inhabited (μ (ValueIdT β) ι)] [Inhabited τ] : Inhabited (State β ι τ μ) where
  default := {
               accounts    := default
             , choices     := default
             , boundValues := default
             , minTime     := default
             }

export State (accounts choices boundValues minTime)


structure TimeInterval (τ : Type) where
  val : τ × τ
deriving BEq, Repr


structure Environment (τ : Type) where
  timeInterval : TimeInterval τ
deriving Repr

export Environment (timeInterval)


structure Money (β ι : Type) where
  val : TokenT β × ι
deriving Repr


structure Payment (β ι : Type) where
  account : AccountId β
  payee   : Payee β
  money   : Money β ι
deriving Repr


end Marlowe.Language.State



namespace Marlowe.Language.Class


class ABool (ω : Type) where
  atrue      : ω
  afalse     : ω
  not        : ω → ω
  and        : ω → ω → ω
  or         : ω → ω → ω

instance [ABool ω] : Inhabited ω where
  default := ABool.afalse

export ABool (atrue afalse)


class ACond (ω ι : Type) where
  ifThenElse : ω → ι → ι → ι


class AInt (ι : Type) where
  zero : ι
  neg : ι → ι
  add : ι → ι → ι
  sub : ι → ι → ι
  mul : ι → ι → ι
  div : ι → ι → ι

instance [AInt ι] : Inhabited ι where
  default := AInt.zero


class APOSIXTime (τ : Type) (ι : outParam Type) where
  toAInt : τ → ι
  fromAInt : ι → τ

instance [AInt ι] [APOSIXTime τ ι] : Inhabited τ where
  default := APOSIXTime.fromAInt AInt.zero


class AEq (α : Type) (ω : outParam Type) where
  aeq : α → α → ω

instance [BEq α] : AEq α Bool where
  aeq := BEq.beq


class AOrd (α : Type) (ω : outParam Type) where
  lt : α → α → ω
  le : α → α → ω


notation:10 o " A? " x " A: " y => ACond.ifThenElse o x y

infixl:35 " A&& " => ABool.and
infixl:30 " A|| " => ABool.or
infix:50  " A<= " => AOrd.le
infix:50  " A≤ "  => AOrd.le
infix:50  " A< "  => AOrd.lt
infix:50  " A== " => AEq.aeq
infixl:65 " A+ "  => AInt.add
infixl:65 " A- "  => AInt.sub
infixl:70 " A* "  => AInt.mul
infixl:70 " A/ "  => AInt.div
prefix:100 "A-"   => AInt.neg
prefix:max "A!"   => ABool.not


class AList (σ : Type → Type) (k : Type) where
  empty : σ k
  cons : k → σ k → σ k

instance [AList σ k] : Inhabited (σ k) where
  default := AList.empty


class AMap (μ : Type → Type → Type) (k v : Type) (σ : outParam (Type → Type)) (ω : outParam Type) where
  empty : μ k v
  toList : μ k v → σ (k × v)
  fromList [AEq k ω] : σ (k × v) → μ k v 
  insert [AEq k ω] : k → v → μ k v → μ k v
  member [ABool ω] [AEq k ω] : k → μ k v → ω
  lookup [AEq k ω] [Inhabited v] : k → μ k v → v

instance [AMap μ k v σ ω] : Inhabited (μ k v) where
  default := AMap.empty


end Marlowe.Language.Class

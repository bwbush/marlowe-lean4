

namespace Plutus.V1.Ledger.Time


structure POSIXTime :=
  getPOSIXTime : Int
deriving BEq, Inhabited, Ord, Repr


instance : LT POSIXTime where
  lt x y := x.getPOSIXTime < y.getPOSIXTime


def POSIXTime.decLt (x y : POSIXTime) : Decidable (x < y) :=
  match x, y with
  | ⟨x⟩, ⟨y⟩ => inferInstanceAs $ Decidable (x < y)

instance (x y : POSIXTime) : Decidable (x < y) :=
  POSIXTime.decLt x y


instance : LE POSIXTime where
  le x y := x.getPOSIXTime <= y.getPOSIXTime


def POSIXTime.decLe (x y : POSIXTime) : Decidable (x <= y) :=
  match x, y with
  | ⟨x⟩, ⟨y⟩ => inferInstanceAs $ Decidable (x <= y)

instance (x y : POSIXTime) : Decidable (x <= y) :=
  POSIXTime.decLe x y


export POSIXTime (getPOSIXTime)


end Plutus.V1.Ledger.Time

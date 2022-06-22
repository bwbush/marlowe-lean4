

namespace Marlowe.Semantics.Primitives


def ByteString :=
  if true
    then String
    else ByteArray

instance : Ord ByteString where
  compare (x : String) (y : String) := compare x y

instance : Repr ByteString where
  reprPrec s _ := s.quote


def Integer := Int

deriving instance Ord, Repr for Integer


def POSIXTime := Int

deriving instance Ord, Repr for POSIXTime


end Marlowe.Semantics.Primitives

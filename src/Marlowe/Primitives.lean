

namespace Marlowe.Primitives


def ByteString :=
  if true
    then String
    else ByteArray

instance : BEq ByteString where
  beq (x : String) (y : String) :=  x == y


instance : Inhabited ByteString where
  default := ""

instance : Ord ByteString where
  compare (x : String) (y : String) := compare x y

instance : Repr ByteString where
  reprPrec s _ := s.quote


def Integer := Int

deriving instance BEq, Inhabited, Ord, Repr for Integer

def fromInteger (i : Integer) : Int := i


def POSIXTime := Int

deriving instance BEq, Inhabited, Ord, Repr for POSIXTime

def fromPOSIXTime (i : POSIXTime) : Int := i


end Marlowe.Primitives

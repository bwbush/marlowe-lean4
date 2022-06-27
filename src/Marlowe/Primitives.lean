

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


structure Integer :=
  toInt : Int
deriving BEq, Inhabited, Ord, Repr

-- FIXME: Implement comparison operators.

def integer (i : Int) : Integer := {toInt := i}


structure POSIXTime :=
  toInt : Int
deriving BEq, Inhabited, Ord, Repr

-- FIXME: Implement comparison operators.

def posixTime (i : Int) : POSIXTime := {toInt := i}


end Marlowe.Primitives

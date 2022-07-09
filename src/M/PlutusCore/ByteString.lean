

namespace PlutusCore


structure ByteString where
  bytes : ByteArray
deriving Inhabited


instance : BEq ByteString where
  beq x y := x.bytes.toList == y.bytes.toList


private def compareList [Ord a] : List a → List a → Ordering
  | []     , []      => Ordering.eq
  | []     , _       => Ordering.lt
  | _      , []      => Ordering.gt
  | x :: xs, y :: ys => match compare x y with
                          | Ordering.eq => compareList xs ys
                          | o           => o 

instance : Ord ByteString where
  compare x y := compareList x.bytes.toList y.bytes.toList


private def toHexDigit : UInt8 → Char
  |  0 => '0'
  |  1 => '1'
  |  2 => '2'
  |  3 => '3'
  |  4 => '4'
  |  5 => '5'
  |  6 => '6'
  |  7 => '7'
  |  8 => '8'
  |  9 => '9'
  | 10 => 'a'
  | 11 => 'b'
  | 12 => 'c'
  | 13 => 'd'
  | 14 => 'e'
  | 15 => 'f'
  |  _ => '?'

private def toHexByte (x : UInt8) : String :=
  let hi := UInt8.div x 16
  let lo := UInt8.mod x 16
  toString (toHexDigit hi) ++ toString (toHexDigit lo)

private def toHexString (x : ByteArray) : String :=
  let f (bs : String) (b : UInt8) : String := bs ++ toHexByte b
  x.foldl f default 

instance : ToString ByteString where
  toString x := toHexString x.bytes


instance : Repr ByteString where
  reprPrec x _ := toString x


namespace ByteString

  def fromList : List UInt8 → ByteString :=
    mk ∘ ByteArray.mk ∘ Array.mk

  def fromString : String → ByteString :=
    mk ∘ String.toUTF8

end ByteString


end PlutusCore

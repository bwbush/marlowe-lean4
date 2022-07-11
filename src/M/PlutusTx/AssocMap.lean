

namespace PlutusTx.AssocMap


structure Map (k v) where
  toList : List (k × v)
deriving Inhabited, Repr


private def insertPair [BEq k] : List (k × v) → (k × v) → List (k × v)
  | []                       , entry        => [entry]
  | (key', value') :: entries, (key, value) => if key == key'
                                                 then (key , value ) :: entries
                                                 else (key', value') :: insertPair entries (key, value)


namespace Map

  def empty : Map k v :=
    mk []

  def fromList [BEq k] : List (k × v) → Map k v :=
    mk ∘ List.foldl insertPair default

  def insert [BEq k] : k → v → Map k v → Map k v
    | key, value, mk entries => mk $ insertPair entries (key, value)


  def lookup [BEq k] [Inhabited v] : k → Map k v → v
    | _  , mk []                          => default
    | key, mk ((key', value') :: entries) => if key == key'
                                               then value'
                                               else lookup key $ mk entries

  def member [BEq k]: k → Map k v → Bool
    | _  , mk []                     => false
    | key, mk ((key', _) :: entries) => if key == key'
                                          then true
                                          else member key $ mk entries

end Map


end PlutusTx.AssocMap

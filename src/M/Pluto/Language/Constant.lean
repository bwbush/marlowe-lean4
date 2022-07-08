
import M.PlutusCore


namespace Pluto.Language


open PlutusCore (ByteString Data)


universe u


inductive ConstantT (ann : Type u) where
  | I : ann → Int → ConstantT ann
  | S : ann → ByteString → ConstantT ann
  | T : ann → String → ConstantT ann
  | U : ann → ConstantT ann
  | B : ann → Bool → ConstantT ann
  | D : ann → Data → ConstantT ann
deriving Repr

export ConstantT (I S T U B)


end Pluto.Language


namespace Pluto.Language


open PlutusCore


universe u


inductive ConstantT (ann : Type u) where
  | I : ann → Int → ConstantT ann
  | S : ann → String → ConstantT ann  -- FIXME: Use `ByteArray`.
  | T : ann → String → ConstantT ann
  | U : ann → ConstantT ann
  | B : ann → Bool → ConstantT ann
  | D : ann → Data → ConstantT ann  -- FIXME: Add plutus data.
deriving Repr

export ConstantT (I S T U B)


end Pluto.Language

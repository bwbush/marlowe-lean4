

import M.Pluto.Language.Builtin
import M.Pluto.Language.Constant


namespace Pluto.Language


open PlutusCore (ByteString Data)


universe u


structure Name where
  getName : String
deriving Repr

instance : ToString Name where
  toString := Name.getName

export Name (getName)


mutual

  inductive Term (ann : Type u) where
    | Var        : ann → Name → Term ann
    | Lambda     : ann → List Name → Term ann → Term ann
    | Apply      : ann → Term ann → Term ann → Term ann
    | Force      : ann → Term ann → Term ann
    | Delay      : ann → Term ann → Term ann
    | Constant   : ann → ConstantT ann → Term ann
    | Builtin    : ann → BuiltinT → Term ann
    | Error      : ann → Term ann
    | Let        : ann → List (BindingT ann) → Term ann → Term ann
    | IfThenElse : ann → Term ann → Term ann → Term ann → Term ann
  deriving Repr

  inductive BindingT (ann : Type u) where
    | Binding : ann → Name → Term ann → BindingT ann
  deriving Repr

end

export Term (Var Lambda Apply Force Delay Constant Builtin Error Let IfThenElse)

export BindingT (Binding)


open BindingT Term

mutual

  def showBinding : BindingT ann → String
    | Binding _ name term => toString name ++ " = " ++ showTerm term

  def showBindings : List (BindingT ann) → String
    | []      => ""
    | [b]     => showBinding b
    | b :: bs => showBinding b ++ "; " ++ showBindings bs

  def showTerm' (term : Term ann) : String :=
    let rec hasSpace : List Char → Bool
              | []        => false
              | ' ' :: _  => true
              | _   :: xs => hasSpace xs
    let s := showTerm term
    if hasSpace s.toList
      then "(" ++ s ++ ")"
      else s

  def intercalate' : List Name → String
    | []      => ""
    | [x]     => toString x
    | x :: xs => toString x ++ " " ++ intercalate' xs

  def showTerm : Term ann → String
    | Var _ name                     => toString name
    | Lambda _ names term            => "(\\" ++ intercalate' names ++ " -> " ++ showTerm term ++ ")"
    | Apply _ term term'             => showTerm' term ++ " " ++ showTerm' term'
    | Force _ term                   => "! " ++ showTerm' term
    | Delay _ term                   => "# " ++ showTerm' term
    | Constant _ cons                => toString cons
    | Builtin _ builtin              => toString builtin
    | Error _                        => "Error"
    | Let _ []        term           => "let in " ++ showTerm term
    | Let _ bindings term            => "let " ++ showBindings bindings ++ " in " ++ showTerm term
    | IfThenElse _ term term' term'' => "if " ++ showTerm term ++ " then " ++ showTerm term' ++ " else " ++ showTerm term''
 
end

instance : ToString (BindingT ann) where
  toString := showBinding

instance : ToString (Term ann) where
  toString := showTerm


structure Program (ann : Type u) where
  unProgram : Term ann
deriving Repr

instance : ToString (Program ann) where
  toString := toString ∘ Program.unProgram

export Program (unProgram)


end Pluto.Language

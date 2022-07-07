
import Pluto.Language.Builtin
import Pluto.Language.Constant


namespace Pluto.Language


universe u


structure Name where
  getName : String
deriving Repr

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


structure Program (ann : Type u) where
  unProgram : Term ann
deriving Repr

export Program (unProgram)


end Pluto.Language

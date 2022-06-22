

import Marlowe.Language


namespace Marlowe.Semantics.Evaluate

open Marlowe.Language.Contract
open Marlowe.Language.State


def evaluate : Environment → State → Value → Int
  | _, _, _ => 0


def observe : Environment → State → Observation → Bool
  | _, _, _ => false


end Marlowe.Semantics.Evaluate

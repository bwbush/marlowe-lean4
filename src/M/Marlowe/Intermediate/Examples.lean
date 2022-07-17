

import M.Marlowe.Intermediate.Operate
import M.Marlowe.Language
import M.Plutus


namespace Marlowe.Intermediate.Examples


open Marlowe.Intermediate
open Marlowe.Language.Contract
open Marlowe.Language.Input
open Plutus.V1.Ledger.Time


def testIntermediate : Op :=
 operate
   [
     NormalInput INotify
   ]
   (
     When
       [
         Case (Notify TrueObs)
           $ Assert TrueObs
             Close
       ]
       (POSIXTime.mk 4)
       Close
   )
   NewContract
   Wait

#eval testIntermediate


end Marlowe.Intermediate.Examples

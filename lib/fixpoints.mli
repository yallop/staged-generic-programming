open Common
open Classes_
open Partially_static

val gfixQ_ : ('v code genericQ_ -> 'v code genericQ_) -> 'v code genericQ_
val gfixT_ : (genericT_ -> genericT_) -> genericT_
val gfixM_ : {M:MONAD_} -> (M.t genericM_ -> M.t genericM_) -> M.t genericM_

val gfixQ2_ : {R:PS} -> (R.t genericQ_ -> R.t genericQ_) -> R.t genericQ_

val gfixQ2_ps_ : {R:PS} -> {M:MONOID with type t = R.t} ->
  (R.t genericQ_ -> R.t genericQ_) -> R.t genericQ_

val gfixQ3_ : {P:PS} ->
              (P.t genericQ_ -> ({D:DATA_} -> D.t_ -> P.t)) ->
              P.t genericQ_

val gfixQ3_ps_ : {P:PS} -> {M:MONOID with type t = P.t} ->
              (P.t genericQ_ -> ({D:DATA_} -> D.t_ -> P.t)) ->
              M.t genericQ_

open Metasyb_classes_
open Partially_static

val generateT : {D:DATA_} -> genericT_ -> (D.t -> D.t) code
val generateQ : {D:DATA_} -> 'u code genericQ_ -> (D.t -> 'u) code

val generateQps : {D:DATA_} -> {P:PS} -> P.t genericQ_ -> (D.t -> P.sta) code

val instantiateT : {D:DATA_} -> genericT_ -> D.t -> D.t
val instantiateQ : {D:DATA_} -> 'u code genericQ_ -> D.t -> 'u

val instantiateQps : {P:PS} -> {D:DATA_} -> P.t genericQ_ -> D.t -> P.sta

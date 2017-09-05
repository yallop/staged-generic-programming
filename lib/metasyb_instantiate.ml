open Metasyb_classes_
open Partially_static

let generateT {D: DATA_} (f : genericT_) =
  Metasyb_bindings.let_locus @@ fun () -> .< fun x -> .~(f .<x>.) >.

let generateQ {D: DATA_} (q : 'u genericQ_) =
  Metasyb_bindings.let_locus @@ fun () -> .< fun x -> .~(q .<x>.) >.

let generateQps {D:DATA_} {P:PS} (q : P.t genericQ_) =
  Metasyb_bindings.let_locus @@ fun () -> .< fun x -> .~(cd (q .<x>.)) >.

let instantiateT {D: DATA_} f = Runcode.run (generateT {D} f)

let instantiateQ {D: DATA_} q = Runcode.run (generateQ {D} q)

let instantiateQps {P:PS}  {D: DATA_} q = Runcode.run (generateQps {D} {P} q)

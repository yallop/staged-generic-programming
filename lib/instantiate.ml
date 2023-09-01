open Classes_
open Partially_static

let generateT {D: DATA_} (f : genericT_) =
  Bindings_.let_locus @@ fun () -> .< fun x -> .~(f .<x>.) >.

let generateQ {D: DATA_} (q : 'u genericQ_) =
  Bindings_.let_locus @@ fun () -> .< fun x -> .~(q .<x>.) >.

let generateQps {D:DATA_} {P:PS} (q : P.t genericQ_) =
  Bindings_.let_locus @@ fun () -> .< fun x -> .~(cd (q .<x>.)) >.

let instantiateT {D: DATA_} f = Runcode.run (generateT {D} f)

let instantiateQ {D: DATA_} q = Runcode.run (generateQ {D} q)

let instantiateQps {P:PS}  {D: DATA_} q = Runcode.run (generateQps {D} {P} q)

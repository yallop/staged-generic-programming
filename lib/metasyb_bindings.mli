(*
  Safe and modular let-insertion for MetaOCaml.

  See http://okmij.org/ftp/meta-programming/#let-insert

  Example:
     let_locus (fun () ->
      .< 1 + .~(genlet .< 2 >.) >.
  ~>
     .< let x = 2 in
        1 + x >.

  [This interface file added by Jeremy Yallop, March 2015.]
 *)

val genlet : 'a code -> 'a code
val genletrec1 : (('a -> 'b) code -> 'a code -> 'b code) -> ('a -> 'b) code
val let_locus : (unit -> 'w code) -> 'w code

open Metasyb_classes_
open Partially_static
val case_locus : {P:PS} -> (unit -> P.t) -> P.t
val reify : {P:PS} -> {D:DATA_} -> D.t code -> (D.t_ -> P.t) -> P.t

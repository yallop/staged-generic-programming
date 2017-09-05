(* Basic definitions, not really specific to SYB *)

(* A sum type *)
type ('a,'b) either = Left of 'a | Right of 'b


(* Monoids (for generic queries) *)
module type MONOID =
sig
  type t
  val zero : t
  val (<+>) : t -> t -> t
end

let zero {M:MONOID} () = M.zero
let (<+>) {M:MONOID} a b = M.(a <+> b)

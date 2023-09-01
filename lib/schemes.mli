open Common
open Classes
open Classes_
open Partially_static

(** Apply a transformation everywhere in bottom-up manner *)
val everywhere_ : genericT_ -> genericT_

(** Apply a transformation everywhere in top-down manner *)
val everywhere'_ : genericT_ -> genericT_

(** Variation on everywhere with an extra stop condition *)
val everywhereBut_ : bool code genericQ_ -> genericT_ -> genericT_

(** Monadic variation on everywhere *)
val everywhereM_ : {M:MONAD_} -> M.t genericM_ -> M.t genericM_

(** Apply a monadic transformation at least somewhere *)
(* val somewhere : (MonadPlus m, Data a) => GenericM m -> a -> m a *)

(** Summarise all nodes in top-down, left-to-right order *)
val everything_ : {P:PS} -> {M:MONOID with type t = P.t} -> M.t genericQ_ -> M.t genericQ_

(** Variation of "everything" with an added stop condition *)
val everythingBut_ : ('r code -> 'r code -> 'r code) -> ('r * bool) code genericQ_ -> 'r code genericQ_

(** Get a list of all entities that meet a predicate *)
val listify_ : {R:TYPEABLE} -> (R.t code -> bool code) -> R.t ps_list_code_monoid genericQ_

(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
val synthesize_ : 's code -> ('t code -> 's code -> 's code) -> ('s -> 't) code genericQ_ -> 't code genericQ_

(** Compute size of an arbitrary data structure *)
val gsize_ : PS_int_monoid.t genericQ_

(** Count the number of immediate subterms of the given term *)
val glength_ : PS_int_monoid.t genericQ_

(** Determine depth of the given term *)
val gdepth_ : Tropical_nats.t genericQ_

(** Determine the number of all suitable nodes in a given term *)
val gcount_ : PS_bool.t genericQ_ -> ps_int_plus genericQ_

(** Determine the number of all nodes in a given term *)
val gnodecount_ : ps_int_plus genericQ_

(** Determine the number of nodes of a given type in a given term *)
val gtypecount_ : {X:TYPEABLE} -> X.t -> ps_int_plus genericQ_

(** Find (unambiguously) an immediate subterm of a given type *)
val gfindtype_ : {X:TYPEABLE} -> X.t option code genericQ_

(** Generic show *)
val gshow_ : ps_string genericQ_

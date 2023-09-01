open Common
open Classes_
open Fixpoints
open Bindings_
open Partially_static

module type TYPEABLE = Classes.TYPEABLE

(* Various utilities *)
let unjust l o = match o with Some x -> x :: l | None -> l
let singleton = function [s] -> Some s | _ -> None

module Code_ps (S: sig type t end) : PS with type sta = S.t and type t = S.t code =
struct
  type sta = S.t
  type t = S.t code
  let cd c = c
  let dyn c = c
  let now _ = None
  let eq _ _ = UNKNOWN
end

module PS_of_data (D:DATA_) : PS =
struct
  module rec R: PS with type t = D.t_ and type sta = D.t = struct
    type t = D.t_
    type sta = D.t
    let eq _ _ = UNKNOWN
    let now _ = None
    let cd = D.reflect
    let dyn c = D.reify {R} c (fun x -> x)
  end
  include R
end


let msum {M:MONOID} l = List.fold_left M.(<+>) M.zero l

(** Apply a transformation everywhere in bottom-up manner *)
let everywhere_ (f : genericT_) =
  gfixT_ (fun self {X:DATA_} x ->
  let implicit module P = Code_ps(struct type t = X.t end) in
  cd (reify x @@ fun x -> f (reflect (gmapT_ self x))))


(** Apply a transformation everywhere in top-down manner *)
let everywhere'_ (f : genericT_) =
  gfixT_ (fun (self : genericT_) {X:DATA_} x ->
      let g : genericT_ = (fun {E:DATA_} y -> self (f y)) in
      let implicit module P = Code_ps(struct type t = X.t end) in
      cd (reify x @@ fun x ->
      reflect (gmapT_ {X} g x)))


(** Variation on everywhere with an extra stop condition *)
let everywhereBut_ (stop : bool code genericQ_) (f : genericT_) =
  gfixT_ (fun self {X:DATA_} x ->
      let implicit module P = Code_ps(struct type t = X.t end) in
      cd (reify x @@ fun x' ->
      .< if .~(stop x) then .~x else .~(f (reflect (gmapT_ {X} self x'))) >.))


(** Monadic variation on everywhere *)
let everywhereM_ : {M:MONAD_} -> M.t genericM_ -> M.t genericM_ =
  fun {M:MONAD_} (f : M.t genericM_) ->
    gfixM_ (fun (self : M.t genericM_) {X:DATA_} x ->
        gmapM_ self x >>== fun x' -> f x')

(** Summarise all nodes in top-down, left-to-right order *)
let everything_ {P:PS} {M:MONOID with type t = P.t} (g : _ genericQ_) =
  gfixQ2_ps_ {P} {M}
      
 (fun self {X: DATA_} x ->
      let f = g x in
      let rec crush u = function [] -> u | x :: xs -> crush (M.(<+>) u  x) xs in
      (reify x @@ fun x ->
      (crush f (gmapQ_ self x))))


(** Variation of "everything" with an added stop condition *)
let everythingBut_ (type r) ((@) : r code -> r code -> r code) (stop : _ genericQ_) =
  gfixQ_ (fun self {X: DATA_} x ->
      let implicit module P = Code_ps(struct type t = r end) in
      cd {P} (reify x @@ fun x' ->
      .< match .~(stop x) with
      | v, true -> v
      | v, false -> .~(List.fold_left (@) .<v>. (gmapQ_ self x')) >.))


(** Get a list of all entities that meet a predicate *)
let listify_ {R:TYPEABLE} (p : R.t code -> bool code)=
  let implicit module P = PS_code_list(struct type t = R.t end) in
  everything_ {P} {P}
     (mkQ_ P.zero (fun x -> reify {P} {Instances_.Data_bool}  (p x) @@ function
                      false -> P.zero
                    | true -> P.(sta [x])))

(** Bottom-up synthesis of a data structure;
    1st argument z is the initial element for the synthesis;
    2nd argument o is for reduction of results from subterms;
    3rd argument f updates the synthesised data according to the given term
*)
let synthesize_ (type s) z (o : s code -> _ -> _) (f : _ genericQ_) =
  gfixQ_ (fun self {X: DATA_} x ->
      let implicit module P = Code_ps(struct type t = s end) in
      cd {P} (reify x @@ fun x' ->
      .< .~(f x) .~(List.fold_right o (gmapQ_ self x') z) >.))


(** Compute size of an arbitrary data structure *)
let gsize_ = 
    let open PS_int_monoid in
    gfixQ3_ {PS_int_monoid} (fun self {D:DATA_} v ->
    sta 1 <+> (msum {PS_int_monoid}) (gmapQ_ self v))


(** Count the number of immediate subterms of the given term *)
let glength_ {T: DATA_} x =
    let open PS_int_monoid in
    reify {PS_int_monoid} x @@ fun x ->
    sta (List.length (gmapQ_ (fun {Z:DATA_} _ -> .< () >.) x))


(** Determine depth of the given term *)
let gdepth_ =
  let open Tropical_nats in
  gfixQ3_ {Tropical_nats} (fun self {D: DATA_} x ->
  sta 1 <+> List.fold_left max zero (gmapQ_ self x))


(** Determine the number of all suitable nodes in a given term *)
let gcount_ (p : PS_bool.t genericQ_) {T: DATA_} x =
  let implicit module M = PS_int_monoid in
  everything_ {M} {M}
    (fun {X: DATA_} x ->
       match p x with
       | Sta false -> M.sta 0
       | Sta true -> M.sta 1
       | Dyn y -> reify {M} {Instances_.Data_bool} y @@ function
           true -> M.sta 1
         | false -> M.sta 0)
    x


(** Determine the number of all nodes in a given term *)
let gnodecount_ {X: DATA_} x = gcount_ (fun {Y: DATA_} _ -> PS_bool.tt) x


(** Determine the number of nodes of a given type in a given term *)
let gtypecount_ {X:TYPEABLE} _x = gcount_ (mkQ_ (PS_bool.ff) (fun _ -> PS_bool.tt))


(** Find (unambiguously) an immediate subterm of a given type *)
let gfindtype_ {X:TYPEABLE} {D: DATA_} x =
   let implicit module P = Code_ps(struct type t = X.t option end) in
   cd {P} (reify {P} x @@ fun x ->
   let dynlist l = List.fold_right (fun h t -> .<.~h :: .~t>.) l .<[]>. in
  .< singleton (List.fold_left unjust []
               .~(dynlist (gmapQ_ (mkQ_ .<None>. (fun c -> .<Some .~c>.)) x))) >.)


(** Generic show *)
let gshow_ = gfixQ2_ {PS_string_monoid} (fun self {D:DATA_} v ->
  (reify {PS_string_monoid} {D} v @@ fun v ->
    (Constructors_.string_of_applied_constructor
    (constructor_ v) (gmapQ_ self v))))

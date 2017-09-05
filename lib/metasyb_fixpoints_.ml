open Higher
open Syb_common
open Syb_classes
open Metasyb_classes_

let letrec k =
  let r = Metasyb_bindings.genlet (.< ref (fun _ -> assert false) >.) in
  let _ : unit code = Metasyb_bindings.genlet (.<.~r := .~(k .< ! .~r >.) >.) in
  .< ! .~r >.

implicit module Typeable_of_data{A:DATA_} = Syb_instances.Typeable_of_data{A}

module Memofix  (Elem: sig type (_,_) t end) =
struct
  type _ t =
      Nil : 'a t
    | Cons : (module TYPEABLE with type t = 'b) * ('b -> ('a, 'b) Elem.t) code * 'a t -> 'a t

  let empty () = ref Nil

  let add {T:TYPEABLE} t c = t := Cons ((module T), c, !t)

  let rec lookup : type a. {T:TYPEABLE} -> a t -> (T.t -> (a, T.t) Elem.t) code option =
    fun {T: TYPEABLE} -> function
        Nil -> None
      | Cons ((module R), d, rest) ->
        match (=~~=) {T} {R} with
          Some Refl -> Some d
        | None -> lookup rest

  type 'a f = {T: DATA_} -> T.t code -> ('a, T.t) Elem.t code

  let memofix (openf : 'v f -> 'v f) =
    let tbl = empty () in
    let rec result {D: DATA_} x =
      match lookup !tbl, D.peers with
        Some g, _ -> .< .~g .~x >.
      | None, lazy [] ->
         let g = Metasyb_bindings.genlet .< fun y -> .~(openf result .< y >.) >. in
         add tbl g;
         .< .~g .~x >.
      | None, lazy [_] ->
         let g = Metasyb_bindings.genletrec1 @@ fun self ->
                 add tbl self;
                 fun y -> openf result y
         in .< .~g .~x >.
      | None, _  ->
         let g = letrec (fun self ->
                          add tbl self;
                          .< fun y -> .~(openf result .<y>.) >.)
         in .< .~g .~x >.
    in result
end

module T = Memofix(struct type ('a, 't) t = 't end)
let gfixT_ = T.memofix

module Q = Memofix(struct type ('a, 't) t = 'a end)
let gfixQ_ = Q.memofix

module M(N: MONAD_) = Memofix(struct type ('a, 't) t = ('t, N.t) app end)
let gfixM_ {N: MONAD_} = let module M' = M(N) in M'.memofix

open Partially_static

module Memofix2 (R:PS) =
struct
  type 'b elem = Dyn of ('b -> R.sta) code

  type t =
      Nil : t
    | Cons : (module TYPEABLE with type t = 'b) * 'b elem * t -> t

  let empty () = ref Nil

  let add {T:TYPEABLE} t c = t := Cons ((module T), Dyn c, !t)

  let rec lookup : type a. {T:TYPEABLE} -> t -> T.t elem option =
    fun {T: TYPEABLE} -> function
        Nil -> None
      | Cons ((module R), d, rest) ->
        match (=~~=) {T} {R} with
          Some Refl -> Some d
        | None -> lookup rest

  type f = {T: DATA_} -> T.t code -> R.t

  let memofix (openf : f -> f) =
    let tbl = empty () in
    let rec result {D: DATA_} x =
      match lookup !tbl, D.peers with
        Some (Dyn g), _ -> R.dyn .< .~g .~x >.
      | None, lazy [] ->
        openf result {D} x
      | None, lazy [_] ->
         let g = Metasyb_bindings.genletrec1 @@ fun self ->
                 add tbl self;
                 fun y -> R.cd (openf result y)
         in R.dyn .< .~g .~x >.
      | None, _  ->
         let g = letrec (fun self ->
                          add tbl self;
                          .< fun y -> .~(R.cd (openf result .<y>.)) >.)
         in R.dyn .< .~g .~x >.
    in result
end

let gfixQ2_ {R:PS} (q : R.t genericQ_ -> R.t genericQ_) =
  let module M = Memofix2(R) in M.memofix q

let data_mem : {D:DATA_} -> (module DATA_) list -> bool =
  fun {D:DATA_} ->
    let eqty (module D':DATA_) = (=~~=) {D'.Typeable} {D.Typeable} <> None in
    List.exists eqty

include struct

  (* Here's what should happen.

       We maintain an environment E

       fix()
         For each group D₁, D₂, …, Dₙ of peers we try the fixpoint thing.
         if we encounter any other groups we run f() on them and augment the environment appropriately.
         if (assuming every item in the group shrinks to zero) every item in the group shrinks to zero
            then we add (non-recursive) constant bindings to the environment
         otherwise we add normal bindings to the environment
 *)
  module Memofix3(R:PS)(M:MONOID with type t = R.t) =
  struct

    type 'b elem = Dyn of ('b -> R.sta) code | Sta

    type t =
        Nil : t
      | Cons : (module TYPEABLE with type t = 'b) * 'b elem * t -> t

    let empty () = ref Nil

    let add {T:TYPEABLE} t c = t := Cons ((module T), c, !t)

    let rec lookup : type a. {T:TYPEABLE} -> t -> T.t elem option =
      fun {T: TYPEABLE} -> function
          Nil -> None
        | Cons ((module R), d, rest) ->
          match (=~~=) {T} {R} with
            Some Refl -> Some d
          | None -> lookup rest

    let tbl = empty ()

    let is_constant_zero : 'a.('a code -> R.t) -> bool =
      fun q ->
        let open Delimcc in (* actually, exceptions would be fine here *)
        let p = new_prompt () in
        push_prompt p @@ fun () ->
        try
          (* The local 'x' here means that the generated code will not
             be inserted higher (due to scope extrusion).
             (FIXME: this will not work for constant q that does not return zero.) *)
          let _ = .< fun x -> .~(let b = (R.eq (q .< x >.) M.zero = EQ) in shift0 p (fun _k -> b)) >. in
          assert false (* unreachable *)
        with _ -> false (* scope extrusion; code (not constant zero) was generated *)


    let rec gfixQ2_ps_ : (R.t genericQ_ -> R.t genericQ_) -> R.t genericQ_ =
      fun (q : R.t genericQ_ -> R.t genericQ_) {D:DATA_} (y : D.t code) ->
        match lookup {D.Typeable} !tbl with
          Some Sta -> M.zero
        | Some (Dyn g) -> R.dyn .< .~g .~y >.
        | None ->
        let peers = Lazy.force D.peers in
        let s {D':DATA_} (x:D'.t code) : R.t =
          if not (data_mem {D'} peers) then (gfixQ2_ps_ q : R.t genericQ_)  {D'} x
          else M.zero
        in
        match peers with
          [] ->
          if is_constant_zero (q s {D}) then  begin
            M.zero
          end
          else begin
            q (gfixQ2_ps_ q) y
          end
        | _ when ListLabels.for_all peers
              ~f:(fun (module D:DATA_) ->
                  is_constant_zero (q s {D})) ->
          begin
            ListLabels.iter peers
              ~f:(fun (module D:DATA_) ->
                  add {D.Typeable} tbl Sta);
            M.zero
          end
        | [_]  ->
           let g = Metasyb_bindings.genletrec1 @@ fun self ->
                   add tbl (Dyn self);
                   fun y -> R.cd (q (gfixQ2_ps_ q) y)
           in R.dyn .< .~g .~y >.
        | _ ->
           let g = letrec (fun self ->
                            add tbl (Dyn self);
                            .< fun y -> .~(R.cd (q (gfixQ2_ps_ q) .<y>.)) >.)
           in R.dyn .< .~g .~y >.
  end

  let gfixQ2_ps_ {P:PS} {M:MONOID with type t = P.t} (q : P.t genericQ_ -> P.t genericQ_) =
    let module M = Memofix3(P)(M) in
    M.gfixQ2_ps_ q

end

let gfixQ3_ {P:PS} (k : P.t genericQ_ -> ({D:DATA_} -> D.t_ -> P.t)) =
  gfixQ2_ {P} (fun self {D:DATA_} x ->
  Metasyb_bindings.reify {P} {D} x @@ fun x -> k self x)

let gfixQ3_ps_ {P:PS} {M:MONOID with type t = P.t} (k : P.t genericQ_ -> ({D:DATA_} -> D.t_ -> P.t)) =
  gfixQ2_ps_ {P} {M} (fun self {D:DATA_} x ->
  Metasyb_bindings.reify {P} {D} x @@ fun x -> k self x)

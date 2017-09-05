(* Generic let-insertion: 
     genlet code
inserts a let expression to bind 'code' as high as possible --
as high in the scope as still safe (creating no scope extrusion).

*)

(* We will be using delimited control. So we load it up and set up *)
(*
#directory "/home/oleg/Cache/ncaml4/caml-shift/";;
#directory "/usr/local/src/ncaml4/caml-shift/";;
#load "delimcc.cma";;
*)

open Delimcc
open Metasyb_classes_

(* If we are going to use delimited control, we need to tell MetaOCaml,
   by adjusting its stackmark facility -- provide the implementation of
   stackmarks that works with delimcc.
*)
let () =
  Trx.set_with_stack_mark {Trx.stackmark_region_fn = fun body ->
   let p = new_prompt () in
   push_prompt p (fun () -> body (fun () -> is_prompt_set p))}

let from_option = function Some x -> x | None -> failwith "fromoption"

let read_answer r = let v = from_option !r in r := None; v (* for safety *)

(* No stack walking; we only need a single insertion point *)
type let_req = 
  | Done: let_req
  | Let: (* Insert this: *)
          'a code *
          (* And call this continuation with the identifier: *)
          ('a code -> let_req) ->
          let_req
  | LetRec: (* Insert this *)
          (('a -> 'b) code -> 'a code -> 'b code) *
          (* And call this continuation with the identifier: *)
          (('a -> 'b) code -> let_req) ->
          let_req

(* The single prompt for let-insertion *)
let lrp : let_req prompt = new_prompt ()


let genlet (c : 'a code) : 'a code = shift0 lrp (fun k -> Let (c,k))
let genletrec1 f = shift0 lrp (fun k -> LetRec (f,k))

let is_well_scoped : 'a. 'a code -> bool = fun c ->
  try ignore (.<begin ignore .~c; () end>.); true with e -> false

let rec let_locus : type w.(unit -> w code) -> w code = fun body ->
  let r = ref None in
  let rec loop : let_req -> w code = function
    | Done      -> read_answer r
    | Let (c,k) -> .<let tt = .~c in .~(loop (k (.<tt>.)))>.
    | LetRec (c, k) ->
      (* If c calls genlet/genletrec1 then we're outside the handler.
         So we install another handler here. *)
      let_locus @@ fun () ->
      .< let rec r x = .~(c .<r>. .<x>.) in .~(loop (k (.<r>.))) >.
in
  loop @@ push_prompt lrp @@ fun () ->
    r := Some (body ()); Done

open Partially_static

type ('t, 't_) reifyt = {
  reifyt: {P:PS} -> ('t code -> ('t_ -> P.t)  -> P.t)
}

type case_req = 
  | Done: case_req
  | Case : (module PS with type t = 'b
                       and type sta = 'bsta) *
           (_, _, 'b, 'bsta) case_record -> case_req
and ('t, 't_, 'b, 'bsta) case_record = {
  x : 't code;
  letk : 'b Lazy.t * bool -> case_req; (* This lazy is a bit odd *)
  unrolledk : 't_ -> 'b;
  reifyk : {P:PS} -> ('t_ -> P.t) -> P.t;
  reifyt: ('t, 't_) reifyt;
}

(* The single prompt for case-insertion *)
let crp : case_req prompt = new_prompt ()

let send_case_req : type b t t_. {P:PS} -> (t, t_) reifyt -> t code -> (t_ -> P.t) -> P.t * bool =
  fun {P:PS} ({reifyt} as rt) x unrolledk ->
    let reifyk : {P:PS} -> (t_ -> P.t) -> P.t
      = fun {P:PS} k -> reifyt x k in
    if is_prompt_set crp then
      let lazy a, b = shift0 crp (fun letk -> Case ((module P), { x; reifyk; reifyt=rt; letk; unrolledk })) in
      a, b
    else
      (reifyk unrolledk, false)


(* if the continuations are both static and equal then we can replace
   the whole match with just the continuation.*)
type 'pt optreq = OCont of 'pt * (unit -> 'pt optreq) | ODone

(* collapse a list of "continuations" if they're all static and equal *)
let rec collapsek : {P:PS} -> P.t ->  P.t list -> P.t option = fun {P:PS} seed -> function
    [] when P.now seed <> None -> Some seed
  | [] -> None
  | k :: ks when P.now k <> None && P.eq k seed = EQ -> collapsek seed ks
  | _ -> None

let optimized_reify : {P:PS} -> {D:DATA_} -> D.t code -> (D.t_ -> P.t) -> P.t option =
  fun {P:PS} {D:DATA_} x unrolledk ->
    let optp = new_prompt () in
    (* 'handle' accumulates the "continuations" (match branches) in
       'conts' and, when they're all accumluated, collapses if they're
       static and equal *)
    let rec handle conts req : P.t option = match conts, req with
        [], ODone -> None
      | seed :: conts, ODone  -> collapsek seed conts
      | conts, OCont (k, kk) ->
        handle (k :: conts) (kk ())
    in
    handle [] @@ push_prompt optp @@ fun () ->
    let _ = D.reify {P} x 
      (fun (t_ : D.t_) -> 
         let v = unrolledk t_ in
         shift0 optp (fun kk -> OCont (v, kk));
         v) in
    ODone

let reify : {P:PS} -> {D:DATA_} -> D.t code -> (D.t_ -> P.t) -> P.t =
  fun {P:PS} {D:DATA_} x unrolledk ->
    match optimized_reify x unrolledk with
      None -> fst (send_case_req {P} {reifyt=D.reify} x unrolledk)
    | Some c -> c

let rec case_locus : {W:PS} -> (unit -> W.t) -> W.t = fun {W:PS} body ->
  let r = ref None in
  let rec loop : case_req -> W.t = function
    | Done      -> read_answer r
    | Case ((module P), {x; reifyk; letk; unrolledk}) when not (is_well_scoped x) ->
      (* It's not well-scoped here; send it back down, either to the
         next handler or to the original call *)

      (* Without 'lazy' in the letk argument the application 'reifyk unrolledk' fails with
         a scope extrusion failure here *) 
      loop (letk (lazy (reifyk {P} unrolledk), false))
    | Case ((module P),{x; reifyk; letk; unrolledk; reifyt}) ->
      (* try higher *)
      match send_case_req {P} reifyt x unrolledk with
      | (_, false) ->
        (* It didn't go any higher *)
        reifyk {W} @@ fun x ->
        (loop (letk (lazy (unrolledk x), true)))
      | (a, b) -> 
        (* it was inserted higher *)
        loop (letk (lazy a, b))
in
  loop @@ push_prompt crp @@ fun () ->
    r := Some (body ()); Done

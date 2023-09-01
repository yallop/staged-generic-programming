(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

open Higher

(* Equality *)
type (_, _) eql = Refl : ('a, 'a) eql

(* Type representations *)
type _ type_rep = ..

(* Our analogue to the typeable class *)
module type TYPEABLE =
sig
  type t
  val type_rep : t type_rep Lazy.t
  val eqty : 's type_rep -> (t, 's) eql option
end

let (=~~=) {A: TYPEABLE} {B: TYPEABLE} = A.eqty (Lazy.force (B.type_rep))

(* Implicit instances *)
module rec R :
sig
  type    genericT  = {T: R.DATA} -> T.t      -> T.t
  type 'u genericQ  = {T: R.DATA} -> T.t      -> 'u
  type 'c genericFapp =
    < g: 'b. {T: R.DATA} -> (T.t -> 'b, 'c) app -> T.t -> ('b, 'c) app >
  type 'c genericFunit = < u: 'g. 'g -> ('g, 'c) app >
  module type DATA =
  sig
    type t
    val data_name : string Lazy.t
    module Typeable : TYPEABLE with type t = t
    val gmapT  : genericT  -> t      -> t
    val gmapQ  : 'u genericQ  -> t      -> 'u list
    val gfoldl : 'c genericFapp -> 'c genericFunit -> t -> (t, 'c) app
    val constructor : t      -> Constructors.constructor
  end
end = R
include R

let gmapT f {D: DATA} = D.gmapT f

let gmapQ f {D: DATA} = D.gmapQ f

let gfoldl f u {D: DATA} = D.gfoldl f u

let constructor {D: DATA} = D.constructor

let app (type a) (type b)
    (module A:TYPEABLE with type t = a)
    (module B:TYPEABLE with type t = b)
    (g : b -> b) (x : a) : a =
  match (=~~=) {A} {B} with
    Some Refl -> g x
  | _ -> x

let app' (type a) (type b) (type u)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (u:  u) (g : b -> u) (x: a) : u =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> u
    
let mkT {T:TYPEABLE} g : genericT =
  fun {D:DATA} -> app (module D.Typeable) (module T) g

let mkQ {T:TYPEABLE} u g : 'u genericQ =
  fun {D: DATA} x -> app' (module D.Typeable) (module T) u g x

module type MONAD =
sig
  type t
  val return : 'a -> ('a, t) app
  val (>>=) : ('a, t) app -> ('a -> ('b, t) app) -> ('b, t) app
end

let return : 'a. {M:MONAD} -> 'a -> ('a, M.t) app =
  fun {M:MONAD} v -> M.return v

let (>>=) : 'a 'b. {M:MONAD} -> ('a, M.t) app -> ('a -> ('b, M.t) app) -> ('b, M.t) app =
  fun {M:MONAD} m k -> M.(>>=) m k

type 'm genericM = {T: DATA} -> T.t -> (T.t, 'm) app

let gmapM : 'a. {D: DATA} -> {M:MONAD} -> M.t genericM -> D.t -> (D.t, M.t) app =
  fun {D: DATA} {M:MONAD} (f : M.t genericM) v ->
    let k : 'b. {D:DATA} -> (D.t -> 'b, M.t) app -> D.t -> ('b, M.t) app =
      fun {D:DATA} c x ->
        c >>= fun c' ->
        f x >>= fun x' ->
        return (c' x')
    in
    gfoldl
      (object
        method g : 'b. {T: R.DATA} -> (T.t -> 'b, M.t) app -> T.t -> ('b, M.t) app = k
      end)
      (object
        method u : 'g. 'g -> ('g, M.t) app = M.return
      end)
      v

let appM (type a) (type b) (type m)
    (module M : MONAD with type t = m)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (g : b -> (b, M.t) app) (x: a) : (a, M.t) app =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> M.return x

let mkM : {M:MONAD} -> {B:TYPEABLE} -> (B.t -> (B.t, M.t) app) ->
          M.t genericM =
  fun {M:MONAD} {B:TYPEABLE} (k : (B.t -> (B.t, M.t) app)) {A:DATA} (x: A.t) ->
    appM (module M) (module A.Typeable) (module B) k x

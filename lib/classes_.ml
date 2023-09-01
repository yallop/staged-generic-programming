(* SYB-style equality, using extensible variants to avoid the unsafe cast. *)

open Higher
open Partially_static
open Classes
open Constructors_

module type MONAD_ =
sig
  type t

  val return_ : 'a code -> ('a, t) app code
  val (>>==)  : ('a, t) app code -> ('a code -> ('b, t) app code) -> ('b, t) app code
end

let return_ : {M:MONAD_} -> 'a code -> ('a, M.t) app code =
  fun {M:MONAD_} v -> M.return_ v

let (>>==)  : {M:MONAD_} ->
              ('a, M.t) app code -> ('a code -> ('b, M.t) app code) -> ('b, M.t) app code =
  fun {M:MONAD_} m k -> M.(>>==) m k

(* Implicit instances *)
module rec R :
sig
  include module type of Classes.R
  type    genericT_ = {T: R.DATA_} -> T.t code -> T.t code
  type 'u genericQ_ = {T: R.DATA_} -> T.t code -> 'u
  type 'c genericFapp_  =
    < g: 'b. {T: R.DATA_} -> (T.t -> 'b, 'c) app code -> T.t code -> ('b, 'c) app code >
  type 'c genericFunit_ = < u: 'g. 'g code -> ('g, 'c) app code >
  type 'm genericM_ = {T: R.DATA_} -> T.t code -> (T.t, 'm) app code
  module type DATA_ =
  sig
    type t
    type t_
    val reify : {P:PS} -> t code ->
                  (t_ -> P.t) ->
                  P.t
    val reflect : t_ -> t code
    include Classes.R.DATA with type t := t
    val gmapT_ : genericT_ -> t_ -> t_
    val gmapQ_ : 'u genericQ_ -> t_ -> 'u list
    val gfoldl_ : 'c genericFapp_ -> 'c genericFunit_ -> t code -> (t, 'c) app code
    val gmapM_ : {M:MONAD_} -> M.t genericM_ -> t code -> (t, M.t) app code
    val constructor_: t_ -> constructor
    val peers : (module R.DATA_) list Lazy.t
  end
end = R
include R

let reflect {D:DATA_} x = D.reflect x

let gmapT_ f {D: DATA_} = D.gmapT_ f

let gmapQ_ f {D: DATA_} = D.gmapQ_ f

let gfoldl_ f u {D: DATA_} = D.gfoldl_ f u

let gmapM_ {M:MONAD_} f {D: DATA_} = D.gmapM_ f

let constructor_ {D: DATA_} = D.constructor_

let app_ (type a) (type b)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (g : b code -> b code) (x : a code) : a code =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> x

let app'_ (type a) (type b) (type u)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (u:  u) (g : b code -> u) (x: a code) : u =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> u
    
let appM_ (type a) (type b) (type m)
    (module M : MONAD_ with type t = m)
    (module A : TYPEABLE with type t = a)
    (module B : TYPEABLE with type t = b)
    (g : b code -> (b, M.t) app code) (x: a code) : (a, M.t) app code =
  match (=~~=) {A} {B} with
  | Some Refl -> g x
  | _         -> M.return_ x


let mkT_  {T:TYPEABLE} g : genericT_ =
  fun {D: DATA_} -> app_ (module D.Typeable) (module T) g

let mkQ_ {T:TYPEABLE} u g : _ genericQ_ =
  fun {D: DATA_} x -> app'_ (module D.Typeable) (module T) u g x

let mkM_ : {M:MONAD_} -> {B:TYPEABLE} -> (B.t code -> (B.t, M.t) app code) ->
          M.t genericM_ =
  fun {M:MONAD_} {B:TYPEABLE} (k : (B.t code -> (B.t, M.t) app code)) {A:DATA_} (x: A.t code) ->
    appM_ (module M) (module A.Typeable) (module B) k x

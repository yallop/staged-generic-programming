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

(* Equality test *)
val (=~~=) : {A:TYPEABLE} -> {B:TYPEABLE} -> (A.t, B.t) eql option

module rec R :
sig
  type    genericT  = {T: R.DATA} -> T.t      -> T.t
  type 'u genericQ  = {T: R.DATA} -> T.t      -> 'u
  type 'c genericFapp  =
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
end
type    genericT  = R.genericT
type 'u genericQ  = 'u R.genericQ
type 'c genericFapp  = 'c R.genericFapp
type 'c genericFunit = 'c R.genericFunit
                          
module type DATA = R.DATA

val gmapT  : genericT  -> genericT

val gmapQ  : 'u genericQ -> 'u list genericQ

val gfoldl  : 'c genericFapp -> 'c genericFunit ->
              {T: DATA} -> T.t -> (T.t, 'c) app

val constructor  : Constructors.constructor genericQ

val mkT  : {T:TYPEABLE} -> (T.t -> T.t) -> genericT

val mkQ  : {T:TYPEABLE} -> 'u -> (T.t -> 'u) -> 'u genericQ

(* Support for monadic traversals.  In SYB there is a default
   definition of gmapM in terms of gfoldl, which we also use here.

   The app type constructor from the Higher library makes it possible
   to abstract over the higher-kinded type t; that abstraction is needed
   in (e.g.) genericM.
*)
module type MONAD =
sig
  type t
  val return : 'a -> ('a, t) app
  val (>>=) : ('a, t) app -> ('a -> ('b, t) app) -> ('b, t) app
end

val return : {M:MONAD} -> 'a -> ('a, M.t) app

val (>>=) : {M:MONAD} -> ('a, M.t) app -> ('a -> ('b, M.t) app) -> ('b, M.t) app

type 'm genericM = {T: DATA} -> T.t -> (T.t, 'm) app

val gmapM : {D: DATA} -> {M:MONAD} -> M.t genericM -> D.t -> (D.t, M.t) app

val mkM : {M:MONAD} -> {B:TYPEABLE} -> (B.t -> (B.t, M.t) app) ->
          M.t genericM

open Higher
open Partially_static
open Constructors_

(** Support for monadic traversals over partially-static data. *) 

module type MONAD_ =
sig
  type t

  val return_ : 'a code -> ('a, t) app code
  val (>>==)  : ('a, t) app code -> ('a code -> ('b, t) app code) -> ('b, t) app code
end

val return_ : {M:MONAD_} -> 'a code -> ('a, M.t) app code

val (>>==)  : {M:MONAD_} ->
              ('a, M.t) app code -> ('a code -> ('b, M.t) app code) -> ('b, M.t) app code

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
end

type    genericT_ = R.genericT_
type 'u genericQ_ = 'u R.genericQ_
type 'c genericFapp_  = 'c R.genericFapp_
type 'c genericFunit_ = 'c R.genericFunit_
type 'm genericM_ = 'm R.genericM_
                          
module type DATA_ = R.DATA_

val reflect : {D:DATA_} -> D.t_ -> D.t code

val gmapT_ : genericT_ -> {D:DATA_} -> D.t_ -> D.t_

val gmapQ_ : 'u genericQ_ -> {D:DATA_} -> D.t_ -> 'u list

val gfoldl_ : 'c genericFapp_ -> 'c genericFunit_ ->
              {T: DATA_} -> T.t code -> (T.t, 'c) app code

val gmapM_ : {M:MONAD_} -> M.t genericM_ -> M.t genericM_

val constructor_ : {D:DATA_} -> D.t_ -> constructor

val mkT_ : {T:Classes.TYPEABLE} -> (T.t code -> T.t code) -> genericT_

val mkQ_ : {T:Classes.TYPEABLE} -> 'u -> (T.t code -> 'u) -> 'u genericQ_

val mkM_ : {M:MONAD_} -> {B:Classes.TYPEABLE} -> (B.t code -> (B.t, M.t) app code) ->
          M.t genericM_

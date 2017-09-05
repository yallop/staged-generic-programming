type partial_equality = EQ | NEQ | UNKNOWN

module type PS =
sig
  type t
  type sta
  (* val sta : sta -> t *)
  val dyn : sta code -> t
  val cd : t -> sta code
  val now : t -> sta option
  val eq : t -> t -> partial_equality
end

(* val sta : {P:PS} -> P.sta -> P.t *)
val dyn : {P:PS} -> P.sta code -> P.t
val cd : {P:PS} -> P.t -> P.sta code

(* Possibly-static data *)
type ('s, 'd) sd = Sta of 's | Dyn of 'd code

module SD (T:sig type t val lift : t -> t code val eq : t -> t -> partial_equality end) :
  PS with type t = (T.t, T.t) sd
      and type sta = T.t

implicit module PS_bool :
sig
  include PS with type t = (bool, bool) sd and type sta = bool
  val tt : t
  val ff : t
end

implicit module PS_pair {A: PS} {B: PS} :
  PS with type sta = (A.sta * B.sta)
      and type t = (A.t * B.t, A.sta * B.sta) sd

type ('sta, 'ps) ps_monoid =
    Empty
  | SCons of 'ps * ('sta, 'ps) ps_monoid
  | DCons of 'sta code list * ('sta, 'ps) ps_monoid

type ps_string = (string,string) ps_monoid

module PS_string_monoid :
sig
  include PS with type t = ps_string and type sta = string
  val sta : sta -> t
  val zero : t
  val (<+>) : t -> t -> t
end

type 'a ps_list = ('a list, 'a list) ps_monoid (* TODO: better *)

module PS_list_monoid (T: sig include PS val sta : sta -> t end) :
sig
  include PS
    with type t = (T.sta list, T.t list) ps_monoid
     and type sta = T.sta list
  val zero : t
  val (<+>) : t -> t -> t
end

type ps_int_plus = {
  sta: int;
  dyn: int code list;
}

module PS_int_monoid :
sig
  include PS with type t = ps_int_plus and type sta = int
  val sta : sta -> t
  val zero : t
  val (<+>) : t -> t -> t
end

type 'a ps_list_code_monoid =
    Empty
  | SCons of 'a code list * 'a ps_list_code_monoid
  | DCons of 'a list code * 'a ps_list_code_monoid

module PS_code_list (A:sig type t end) :
sig
  include PS with type sta = A.t list and type t = A.t ps_list_code_monoid
  val sta : A.t code list -> t
  val zero : t
  val (<+>) : t -> t -> t
end

type tn

module Tropical_nats :
sig
  include PS with type sta = int and type t = tn
  val sta : int -> t
  val zero : t
  val (<+>) : t -> t -> t 
  val max : t -> t -> t
end

open Common
open Classes
open Classes_

(* Data instances for built-in types *)

implicit module Data_int : DATA_
  with type t = int
   and type t_ = int code

implicit module Data_bool : DATA_
  with type t = bool
   and type t_ = bool

implicit module Data_float : DATA_
  with type t = float
   and type t_ = float code

implicit module Data_string : DATA_
  with type t = string
   and type t_ = string code

type ('a, 'r) list_ =
    Nil
  | Cons of 'a code * 'r
implicit module Data_list {A: DATA_} : DATA_
  with type t = A.t list
   and type t_ = (A.t, A.t list code) list_

type ('a, 'b) pair_ = Pair of 'a code * 'b code
implicit module Data_pair {A: DATA_} {B: DATA_} : DATA_
  with type t = A.t * B.t
   and type t_ = (A.t, B.t) pair_

type 'a option_ = None' | Some' of 'a code
implicit module Data_option {A: DATA_} : DATA_ 
  with type t = A.t option
   and type t_ = A.t option_

type ('a, 'b) either_ = Left_ of 'a code | Right_ of 'b code
implicit module Data_either {A: DATA_} {B: DATA_} : DATA_
   with type t = (A.t, B.t) either
   and type t_ = (A.t, B.t) either_

implicit module Typeable_of_data {A: DATA_} : TYPEABLE with type t = A.t

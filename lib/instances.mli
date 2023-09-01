open Common
open Classes

(* Data instances for built-in types *)
implicit module Data_int : DATA with type t = int
implicit module Data_bool : DATA with type t = bool
implicit module Data_float : DATA with type t = float
implicit module Data_string : DATA with type t = string
implicit module Data_list {A: DATA} : DATA with type t = A.t list
implicit module Data_pair {A: DATA} {B: DATA} : DATA with type t = A.t * B.t
implicit module Data_option {A: DATA} : DATA with type t = A.t option
implicit module Data_either {A: DATA} {B: DATA} : DATA with type t = (A.t, B.t) either

module Typeable_int : TYPEABLE with type t = int
module Typeable_bool : TYPEABLE with type t = bool
module Typeable_float : TYPEABLE with type t = float
module Typeable_string : TYPEABLE with type t = string
module Typeable_list {A: TYPEABLE} : TYPEABLE with type t = A.t list
module Typeable_pair {A: TYPEABLE} {B: TYPEABLE} : TYPEABLE with type t = A.t * B.t
module Typeable_option {A: TYPEABLE} : TYPEABLE with type t = A.t option
module Typeable_either {A: TYPEABLE} {B: TYPEABLE} : TYPEABLE with type t = (A.t, B.t) either

implicit module Typeable_of_data {A: DATA} : TYPEABLE with type t = A.t


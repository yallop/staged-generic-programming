open Partially_static

type field_info = { name: string; mut: bool }

type constructor =
  | Cons
  | Tuple of int
  | Record of field_info list
  | Name of string
  | Dyn of string code

val constructor : string -> constructor
val dconstructor : string code -> constructor
val string_of_constructor : constructor -> ps_string
val string_of_applied_constructor : constructor -> ps_string list -> ps_string

type field_info = { name: string; mut: bool }

type constructor =
  | Cons
  | Tuple of int
  | Record of field_info list
  | Name of string

val constructor : string -> constructor
val string_of_constructor : constructor -> string
val string_of_applied_constructor : constructor -> string list -> string

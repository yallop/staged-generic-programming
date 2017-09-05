type field_info = { name: string; mut: bool }

let string_of_field_info = function
    { name; mut=true } -> "mutable "^ name
  | { name; mut=false } -> name

type constructor =
  | Cons
  | Tuple of int
  | Record of field_info list
  | Name of string

let rec to_string = function
    Cons -> "::"
  | Tuple _ -> "()"
  | Record fields -> "{"^ String.concat "; "
                       (List.map string_of_field_info fields)^ "}"
  | Name name -> name

let constructor c = Name c
let string_of_constructor = to_string

let string_of_applied_constructor =
  let pr = Printf.sprintf in
  let rec show c args = match c, args with
      Cons, [x;xs] -> pr "(%s :: %s)" x xs
    | Cons, _ -> assert false
    | Tuple n, args ->
      assert (List.length args = n);
      show_args args
    | Record fields, args ->
      "{"^ show_fields (List.combine fields args) ^ "}"
    | Name name, [] -> name
    | Name name, args -> name ^ " "^ show_args args
  and show_fields fields =
    String.concat ", " (List.map show_field fields)
  and show_field (field, v) =
    string_of_field_info field ^" "^ v
  and show_args = function
      [] -> assert false
    | [x] -> x
    | _ :: _ as args -> "("^ String.concat ", " args ^")"
  in show

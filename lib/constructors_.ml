open Partially_static
implicit module S = PS_string_monoid
let sta = S.sta
let (<+>) = PS_string_monoid.(<+>)

type field_info = { name: string; mut: bool }

let string_of_field_info = function
    { name; mut=true } -> "mutable "^ name
  | { name; mut=false } -> name

type constructor =
  | Cons
  | Tuple of int
  | Record of field_info list
  | Name of string
  | Dyn of string code

let to_string_ps = function
    Cons -> sta "::"
  | Tuple _ -> sta "()"
  | Record fields -> let s = "{"^ String.concat "; "
                               (List.map string_of_field_info fields)^ "}"
    in sta s 
  | Name name -> sta name
  | Dyn d -> dyn d

let constructor c = Name c
let dconstructor c = Dyn c
let string_of_constructor = to_string_ps

let rec concat sep = function
    [] -> sta ""
  | h :: [] -> h
  | h :: t -> h <+> sep <+> concat sep t

let string_of_applied_constructor : constructor -> ps_string list -> ps_string =
  let rec show (c : constructor) (args : ps_string list) : ps_string = match c, args with
      Cons, [x;xs] -> sta "(" <+> x <+> sta " :: " <+> xs <+> sta ")"
    | Cons, _ -> assert false
    | Tuple n, args ->
      assert (List.length args = n);
      show_args args
    | Record fields, args ->
      sta "{" <+> show_fields (List.combine fields args) <+> sta "}"
    | Name name, [] -> sta name
    | Name name, args -> sta name <+> sta " " <+> show_args args
    | Dyn d, [] -> dyn d
    | Dyn d, args -> dyn d <+> sta " " <+> show_args args
  and show_fields fields : ps_string =
    concat (sta ", ") (List.map show_field fields)
  and show_field (field, v) =
    let s = string_of_field_info field in
    sta s <+> sta " " <+> v
  and show_args = function
      [] -> assert false
    | [x] -> x
    | _ :: _ as args -> sta "(" <+> concat (sta ", ") args <+> sta ")"
  in show

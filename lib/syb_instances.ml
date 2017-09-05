open Syb_common
open Syb_constructors
open Syb_classes

(* Some primitive typeable instances *)
type _ type_rep += List : 'a type_rep -> 'a list type_rep
type _ type_rep += Option : 'a type_rep -> 'a option type_rep
type _ type_rep += Pair : 'a type_rep * 'b type_rep -> ('a * 'b) type_rep
type _ type_rep += Either : 'a type_rep * 'b type_rep -> ('a, 'b) either type_rep

module Typeable0_make(T: sig type t end) : TYPEABLE with type t = T.t =
struct
  type _ type_rep += T : T.t type_rep
  type t = T.t
  let eqty : type b. b type_rep -> (t, b) eql option =
    function T -> Some Refl | _ -> None
  let type_rep = lazy T
end

implicit module Typeable_int = Typeable0_make(struct type t = int end)
implicit module Typeable_bool = Typeable0_make(struct type t = bool end)
implicit module Typeable_float = Typeable0_make(struct type t = float end)
implicit module Typeable_string = Typeable0_make(struct type t = string end)

implicit module Typeable_pair{A: TYPEABLE} {B: TYPEABLE} =
struct
  type t = A.t * B.t
  let eqty : type c. c type_rep -> (A.t * B.t, c) eql option = function
      Pair (a, b) ->
      begin match A.eqty a, B.eqty b with
          Some Refl, Some Refl -> Some Refl
        | _ -> None
      end
    | _ -> None

  let type_rep = lazy (Pair (Lazy.force A.type_rep, Lazy.force B.type_rep))
end

implicit module Typeable_either{A: TYPEABLE} {B: TYPEABLE} =
struct
  type t = (A.t, B.t) either
  let eqty : type c. c type_rep -> ((A.t, B.t) either, c) eql option = function
      Either (a, b) ->
      begin match A.eqty a, B.eqty b with
          Some Refl, Some Refl -> Some Refl
        | _ -> None
      end
    | _ -> None

  let type_rep = lazy (Either (Lazy.force A.type_rep, Lazy.force B.type_rep))
end

implicit module Typeable_list{A: TYPEABLE} =
struct
  type t = A.t list
  let eqty : type b. b type_rep -> (A.t list, b) eql option = function
    | List a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep = lazy (List (Lazy.force A.type_rep))
end

implicit module Typeable_option{A: TYPEABLE} =
struct
  type t = A.t option
  let eqty : type b. b type_rep -> (A.t option, b) eql option = function
    | Option a ->
      begin match A.eqty a with
          Some Refl -> Some Refl
        | None -> None
      end
    | _ -> None
  let type_rep = lazy (Option (Lazy.force A.type_rep))
end

implicit module Data_int =
struct
  type t = int
  let data_name = lazy "int"
  module Typeable = Typeable_int
  let gmapT _ x = x
  let gmapQ _ _ = []
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_int x)
end

implicit module Data_bool =
struct
  type t = bool
  let data_name = lazy "bool"
  module Typeable = Typeable_bool
  let gmapT _ x = x
  let gmapQ _ _ = []
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_bool x)
end

implicit module Data_float =
struct
  type t = float
  let data_name = lazy "float"
  module Typeable = Typeable_float
  let gmapT _ x = x
  let gmapQ _ _ = []
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let constructor x = Syb_constructors.constructor (string_of_float x)
end

implicit module Data_string =
struct
  type t = string
  let data_name = lazy "string"
  module Typeable = Typeable_string
  let gmapT _ x = x
  let gmapQ _ _ = []
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) x = u#u x
  let constructor x = Syb_constructors.constructor (Printf.sprintf "%S" x)
end

implicit module Data_list {A: DATA} : DATA with type t = A.t list =
struct
  module rec R : DATA with type t = A.t list =
  struct
    type t = A.t list 
    let data_name = lazy ("list("^Lazy.force A.data_name ^")")
    module Typeable = Typeable_list{A.Typeable}
    let gmapT (f : genericT) (l : t) =
      match l with
        [] -> []
      | x :: xs -> f x :: f {R} xs

    let gmapQ (q : _ genericQ) (l : t) =
      match l with
        [] -> []
      | x :: xs -> [q x; q {R} xs]

    let gfoldl (g : _ genericFapp) (u : _ genericFunit) l =
      match l with
        [] -> u#u []
      | x :: xs -> g#g {R} (g#g (u#u (fun x xs -> x :: xs)) x) xs

    let constructor = function
        [] -> Syb_constructors.constructor "[]"
      | _::_ -> Syb_constructors.Cons
  end
  include R
end

implicit module Data_pair {A: DATA} {B: DATA} : DATA with type t = A.t * B.t =
struct
  type t = A.t * B.t
  let data_name = lazy ("pair("^Lazy.force A.data_name ^","^ Lazy.force B.data_name ^")")

  module Typeable = Typeable_pair{A.Typeable}{B.Typeable}
  let gmapT (f : genericT) ((x, y) : t) = (f x, f y)
  let gmapQ (q : _ genericQ) ((x, y) : t) = [q x; q y]
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) (x, y) =
    g#g {B} (g#g {A} (u#u (fun x y -> (x,y))) x) y
  let constructor _ = Syb_constructors.Tuple 2
end

implicit module Data_option {A: DATA} : DATA with type t = A.t option =
struct
  type t = A.t option
  let data_name = lazy ("option("^Lazy.force A.data_name ^")")
  module Typeable = Typeable_option{A.Typeable}
  let gmapT (f : genericT) (o : t) =
    match o with None -> None | Some x -> Some (f x)
  let gmapQ (q : _ genericQ) (o : t) =
    match o with None -> [] | Some x -> [q x]
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) = function
      None -> u#u None
    | Some x -> g#g {A} (u#u (fun x -> Some x)) x
  let constructor = function
      None -> Syb_constructors.constructor "None"
    | Some _ -> Syb_constructors.constructor "Some"
end

implicit module Data_either {A: DATA} {B: DATA} : DATA with type t = (A.t, B.t) either =
struct
  type t = (A.t, B.t) either

  let data_name = lazy ("either("^Lazy.force A.data_name ^","^ Lazy.force B.data_name ^")")

  module Typeable = Typeable_either{A.Typeable}{B.Typeable}
  let gmapT (f : genericT) : t -> t = function
     Left x -> Left (f x)
   | Right y -> Right (f y)
  let gmapQ (q : _ genericQ) : t -> _ = function
     Left x -> [q x]
   | Right y -> [q y]
  let gfoldl (g : _ genericFapp) (u : _ genericFunit) : t -> _= function
     Left x -> g#g {A} (u#u (fun x -> Left x)) x
   | Right y -> g#g {B} (u#u (fun x -> Right x)) y
  let constructor = function
     Left _ -> Syb_constructors.constructor "Left"
   | Right _ -> Syb_constructors.constructor "Right"
end


implicit module Typeable_of_data{F: DATA} = F.Typeable

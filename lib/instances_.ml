open Common
open Instances
open Classes_
open Constructors_
open Bindings_
open Partially_static

implicit module Data_int =
struct
  include Data_int
  type t_ = int code
  let reify {P:PS} c k = k c
  let reflect c = c
  let gmapT_ _ x = x
  let gmapQ_ _ _ = []
  let gfoldl_ (_ : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let gmapM_ {M:MONAD_} _ (m : t code) = M.return_ m
  let constructor_ x = dconstructor .< string_of_int .~x >.
  let peers = lazy []
end

implicit module Data_bool =
struct
  include Data_bool
  type t_ = bool
  let reify {P:PS} c k = P.dyn .< if .~c then .~(P.cd (k true)) else .~(P.cd (k false)) >.
  let reflect = function false -> .< false >. | true -> .< true >.
  let gmapT_ _ x = x
  let gmapQ_ _ _ = []
  let gfoldl_ (_ : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let gmapM_ {M:MONAD_} _ (m : t code) = M.return_ m
  let constructor_ c = Constructors_.constructor (string_of_bool c)
  let peers = lazy []
end

implicit module Data_float =
struct
  include Data_float
  type t_ = float code
  let reify {P:PS} c k = k c
  let reflect c = c
  let gmapT_ _ x = x
  let gmapQ_ _ _ = []
  let gfoldl_ (_ : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let gmapM_ {M:MONAD_} _ (m : t code) = M.return_ m
  let constructor_ x = dconstructor .< string_of_float .~x >.
  let peers = lazy []
end

implicit module Data_string =
struct
  include Data_string
  type t_ = string code
  let reify {P:PS} c k = k c
  let reflect c = c
  let gmapT_ _ x = x
  let gmapQ_ _ _ = []
  let gfoldl_ (_ : _ genericFapp_) (u : _ genericFunit_) x = u#u x
  let gmapM_ {M:MONAD_} _ (m : t code) = M.return_ m
  let constructor_ x = dconstructor .< Printf.sprintf "%S" .~x >.
  let peers = lazy []
end

type ('a, 'r) list_ =
    Nil
  | Cons of 'a code * 'r

implicit module Data_list {A: DATA_} : DATA_
  with type t = A.t list
   and type t_ = (A.t, A.t list code) list_ =
struct
  module rec R : DATA_
    with type t = A.t list
     and type t_ = (A.t, A.t list code) list_ =
  struct
    include Data_list{A}
    type t_ = (A.t, t code) list_

    let reify : {P:PS} -> t code -> (t_ -> P.t) -> P.t =
      fun {P:PS} c k ->
        P.dyn
        .< match .~c with
          [] -> .~(P.cd (k Nil))
        | h :: t -> .~(P.cd @@ case_locus (fun () -> 
                       (k (Cons (.<h>., .<t>.))))) >.

    let reflect = function
        Nil -> .< [] >.
      | Cons (x, xs) -> .< .~x :: .~xs >.

    let gmapT_ (f : genericT_) = function
        Nil -> Nil
      | Cons (x, xs) -> Cons (f {A} x, f {R} xs)

    let gmapQ_ (q : 'u genericQ_) (l : t_) =
      match l with
        Nil -> []
      | Cons (x, xs) -> [q {A} x; q {R} xs]

    let gfoldl_ (g : 'c genericFapp_) (u : 'c genericFunit_) (l : t code) =
      .< match .~l with
          [] -> .~(u#u .<[]>.)
        | x :: xs -> .~(g#g {R} (g#g (u#u .<(fun x xs -> x :: xs)>.) .<x>.) .<xs>.) >.

    let gmapM_ {M:MONAD_} (f : M.t genericM_) (m : t code) =
      .< match .~m with
          [] -> .~(return_ .<[]>.)
        | x :: xs -> .~(f .<x>. >>== fun y ->
                        f {R} .<xs>. >>== fun ys ->
                        return_ .<.~y :: .~ys>.) >.

    let constructor_ = function
        Nil -> Constructors_.constructor "[]"
      | Cons (_,_) -> Cons

    let peers : (module DATA_) list Lazy.t = lazy [
      (module R);
    ]
  end
  include R
end

type ('a, 'b) pair_ = Pair of 'a code * 'b code

implicit module Data_pair {A: DATA_} {B: DATA_} : DATA_
  with type t = A.t * B.t
   and type t_ = (A.t, B.t) pair_ =
struct
  include Data_pair{A}{B}

  type t_ = (A.t, B.t) pair_

  let reify {P:PS} c k = P.dyn
                         .< let (x, y)= .~c in .~(P.cd @@ case_locus {P} (fun () ->
                                                 (k (Pair (.<x>., .<y>.))))) >.
  let reflect (Pair (x, y)) = .< (.~x, .~y) >.

  let gmapT_ (f : genericT_) (Pair (x, y)) = Pair (f {A} x, f {B} y)

  let gmapQ_  (q : 'u genericQ_) (Pair (x,y)) = [q {A} x; q {B} y]
  let gfoldl_ (g : 'c genericFapp_) (u : 'c genericFunit_) (p : t code) =
     .< let (x, y) = .~p in .~(g#g {B} (g#g {A} (u#u .<fun x y -> (x,y)>.) .<x>.) .<y>.) >.
  let gmapM_ {M:MONAD_} (f : M.t genericM_) (m : t code) =
    .< let (x, y) = .~m in
        .~(f .<x>. >>== fun x' ->
           f .<y>. >>== fun y' ->
           return_ .< (.~x', .~y') >.) >.

  let constructor_ _ = Tuple 2

  let peers = lazy []
end

type 'a option_ = None' | Some' of 'a code

implicit module Data_option {A: DATA_} : DATA_
  with type t = A.t option
   and type t_ = A.t option_ =
struct
  include Data_option{A}

  type t_ = A.t option_
    
  let reify {P:PS} c k =
    P.dyn
    .< match .~c with
        None -> .~(P.cd (k None'))
      | Some x -> .~(P.cd @@ case_locus (fun () ->
                     (k (Some' .<x>.)))) >.
  let reflect = function
      None' -> .< None >.
    | Some' x -> .< Some .~x >.

 let gmapT_ (f : genericT_) = function
     None' -> None'
   | Some' x -> Some' (f {A} x)

  let gmapQ_  (q : 'u genericQ_) = function
      None' -> []
    | Some' x -> [q {A} x]

  let gfoldl_ (g : _ genericFapp_) (u : _ genericFunit_) (o : t code) =
    .< match .~o with
        None -> .~(u#u .<None>.)
      | Some x -> .~(g#g {A} (u#u .<fun x -> Some x>.) .<x>.) >.
  let gmapM_ {M:MONAD_} (f : M.t genericM_) (m : t code) =
    .< match .~m with
        None -> .~(return_ .<None>.)
      | Some x -> .~(f .<x>. >>== fun x' -> return_ .<Some .~x'>.) >.
  let constructor_ = function
      None' -> Constructors_.constructor "None"
    | Some' _ -> Constructors_.constructor "Some"
  let peers = lazy []
end

type ('a, 'b) either_ = Left_ of 'a code | Right_ of 'b code
implicit module Data_either {A: DATA_} {B: DATA_} : DATA_
   with type t = (A.t, B.t) either
   and type t_ = (A.t, B.t) either_ =
struct
  include Data_either{A}{B}

  type t_ = (A.t, B.t) either_

  let reify : {P:PS} -> t code -> (t_ -> P.t) -> P.t =
    fun {P:PS} c k ->
      P.dyn
      .< match .~c with
	Left x -> .~(P.cd @@ case_locus (fun () ->
		      k (Left_ .<x>.)))
      | Right x -> .~(P.cd @@ case_locus (fun () ->
		      k (Right_ .<x>.))) >.

  let reflect = function
      Left_ x -> .< Left .~x >.
    | Right_ y -> .< Right .~y >.

  let gmapT_ (f : genericT_) : t_ -> t_ = function
      Left_ x -> Left_ (f  x)
    | Right_ y -> Right_ (f  y)

  let gmapQ_ (q : 'u genericQ_) (l : t_) =
    match l with
      Left_ x -> [q x]
    | Right_ y -> [q y]

  let gfoldl_ (g : 'c genericFapp_) (u : 'c genericFunit_) (l : t code) =
    .< match .~l with
	Left x -> .~(g#g (u#u .<(fun x -> Left x)>.) .<x>.)
      | Right x -> .~(g#g (u#u .<(fun x -> Right x)>.) .<x>.) >.

  let gmapM_ {M:MONAD_} (f : M.t genericM_) (m : t code) =
    .< match .~m with
	Left x -> .~(f .<x>. >>== fun y -> return_ .< Left .~y >.)
      | Right x -> .~(f .<x>. >>== fun y -> return_ .< Right .~y >.) >.

  let constructor_ = function
      Left_ _ -> Constructors_.constructor "Left"
    | Right_ _ -> Constructors_.constructor "Right"

  let peers = lazy []
end

implicit module Typeable_of_data{F: DATA_} = F.Typeable

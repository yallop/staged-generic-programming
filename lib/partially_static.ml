open Syb_common

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

(* let sta {P:PS} x = P.sta x *)
let dyn {P:PS} x = P.dyn x
let cd {P:PS} x = P.cd x

(* Possibly-static data *)
type ('s, 'd) sd = Sta of 's | Dyn of 'd code

module SD (T:sig type t val lift : t -> t code val eq : t -> t -> partial_equality end) :
sig
  include PS with type t = (T.t, T.t) sd
              and type sta = T.t
  val sta : sta -> t
end
 =
struct
  type t = (T.t, T.t) sd
  type sta = T.t
  let sta s = Sta s
  let dyn d = Dyn d
  let cd = function
      Sta s -> T.lift s
    | Dyn d -> d
  let now = function
      Sta s -> Some s
    | Dyn _ -> None
  let eq l r = match l, r with
      Sta l, Sta r -> T.eq l r
    | _ -> UNKNOWN
end

implicit module PS_bool =
struct include SD(struct type t = bool
    let lift = function
        false -> .<false>.
      | true -> .<true>. 
    let eq l r = if l = r then EQ else NEQ
  end)
  let tt = sta true
  and ff = sta false
end

implicit module PS_pair {A: PS} {B: PS} :
  PS with type sta = (A.sta * B.sta)
      and type t = (A.t * B.t, A.sta * B.sta) sd =
struct
  type sta = (A.sta * B.sta)
  type t = (A.t * B.t, sta) sd
  (* let sta ((x, y) : sta) = Sta (sta x, sta y) *)
  let dyn p = Dyn p
  let cd : t -> sta code = function
    Sta (x, y) -> .< (.~(cd x), .~(cd y)) >.
  | Dyn p -> p
  let now : t -> sta option = function
      Sta (x, y) ->
      begin match A.now x, B.now y with
        Some x, Some y -> Some (x, y)
      | _ -> None
      end
    | Dyn _ -> None
  let eq l r = match l, r with
      Dyn _, _ | _, Dyn _ -> UNKNOWN
    | Sta (l1, r1), Sta (l2, r2) ->
      match A.eq l1 l2 with
        UNKNOWN -> UNKNOWN
      | NEQ -> NEQ
      | EQ ->
        match B.eq r1 r2 with
          UNKNOWN -> UNKNOWN
        | NEQ -> NEQ
        | EQ -> EQ
end

type ('sta, 'ps) ps_monoid =
    Empty
  | SCons of 'ps * ('sta, 'ps) ps_monoid
  | DCons of 'sta code list * ('sta, 'ps) ps_monoid

type ps_string = (string, string) ps_monoid

module PS_monoid
    (T: sig include PS val dyn : unit val sta : sta -> t end)
    (Msta : MONOID with type t = T.t)
    (Mdyn : MONOID with type t = T.sta code) :
sig
  include PS with type t = (T.sta, T.t) ps_monoid and type sta = T.sta
  val zero : t
  val (<+>) : t -> t -> t
end =
struct
  type t = (T.sta, T.t) ps_monoid
  type sta = T.sta
  let zero = Empty
  (* let sta : sta -> t = fun s -> SCons (T.sta s, Empty) *)
  let dyn : _ = fun s -> DCons ([s], Empty)

  let scons (s : T.t) : t -> t = function
    | SCons (s', tl) -> SCons (Msta.(s <+> s'), tl)
    | Empty | DCons _ as r -> SCons (s, r)

  let dcons (s : T.sta code list) : t -> t = function
    | DCons (s', tl) -> DCons (s @ s', tl)
    | Empty | SCons _ as r -> DCons (s, r)

  let rec (<+>) (l : t) (r : t) :t = match l with
      Empty -> r
    | SCons (s, tl) -> scons s (tl <+> r)
    | DCons (s, tl) -> dcons s (tl <+> r)
      
  let rec cd = function
      Empty -> Mdyn.zero
    | SCons (s, Empty) -> T.cd s
    | SCons (s, tl) -> Mdyn.(T.cd s <+> cd tl)
    | DCons ([d], Empty) -> d
    | DCons (h::t, tl) -> Mdyn.(<+>) h  (cd (DCons (t, tl)))
    | DCons ([], t) -> cd t

  let rec now : t -> sta option = function
      Empty -> T.now Msta.zero
    | SCons (s, tl) ->
      begin match now tl with
          None -> None
        | Some s' -> T.now Msta.(s <+> T.sta s')
      end
    | DCons _ -> None

  let rec eq l r = match l, r with
      Empty, Empty -> EQ
    | SCons (x, xs), SCons (y, ys) ->
      begin match T.eq x y with
          EQ -> eq xs ys
        | NEQ -> NEQ
        | UNKNOWN -> UNKNOWN
      end
    | _ -> UNKNOWN

end

module Possibly_static_string =
struct
  type sta = string
  type t = string
  let sta x = x
  let dyn = ()
  let cd (x:string) = .<x>.
  let eq (l : t) (r : t) = if l = r then EQ else NEQ
  let now x = Some x
end

module PS_string_monoid =
struct
  include PS_monoid (* TODO: generalize/abstract this *)
    (Possibly_static_string)
    (struct
      type t = Possibly_static_string.t
      let zero = ""
      let (<+>) = (^)
    end)
    (struct
      type t = string code let zero = .<"">. let (<+>) x y = .< .~x ^ .~y >.
    end)
  let sta s = SCons (s, Empty)
end
type 'a ps_list = ('a list, 'a list) ps_monoid (* TODO: better *)

module PS_list_monoid (T: sig include PS val sta : sta -> t end) =
  PS_monoid
    (struct
      type t = T.t list (* Is this sufficiently dynamic? *)
      type sta = T.sta list
      let rec cd = function
          [] -> .< [] >.
        | h :: t -> .< .~(T.cd h) :: .~(cd t) >.
      let rec eq x y = match x, y with
        [], [] -> EQ
      | x::xs, y::ys ->
        begin match T.eq x y with
            EQ -> eq xs ys
          | NEQ -> NEQ
          | UNKNOWN -> UNKNOWN
        end
      | _ -> NEQ
      let rec now : t -> sta option = function
          [] -> Some []
        | h::t -> match T.now h, now t with
            Some h, Some t -> Some (h :: t)
          | _ -> None
      let dyn = ()
      let sta = List.map T.sta
    end)
    (struct
      type t = T.t list
      let zero = []
      let (<+>) = (@)
    end)
    (struct
      type t = T.sta list code
      let zero = .<[]>.
      let (<+>) x y = .< .~x @ .~ y >.
    end)

type ps_int_plus = {
  sta: int;
  dyn: int code list;
}

module PS_int_monoid =
struct
  type t = ps_int_plus
  type sta = int
  let now = function
    | { dyn = []; sta } -> Some sta
    | { dyn = _::_ } -> None
  let dyn dyn = { dyn = [dyn] ; sta = 0 }
  let cd = function
      { sta; dyn = [] } -> .< sta >.
    | { sta = 0; dyn = h :: t } -> List.fold_right (fun x y -> .< .~x + .~y >.) t h
    | { sta; dyn = h :: t } -> .< sta + .~(List.fold_right (fun x y -> .< .~x + .~y >.) t h) >.
  let sta sta = { sta; dyn = [] }
  let zero = { sta = 0; dyn = [] }
  let (<+>) { sta = lsta; dyn = ldyn } { sta = rsta; dyn = rdyn } =
    { sta = lsta + rsta; dyn = ldyn @ rdyn }
  let eq l r = match l, r with
      { sta = l; dyn = []}, { sta = r; dyn = []} when l = r -> EQ
    | { sta = l; dyn = []}, { sta = r; dyn = []} -> NEQ
    | _ -> UNKNOWN
end

type 'a ps_list_code_monoid =
    Empty
  | SCons of 'a code list * 'a ps_list_code_monoid
  | DCons of 'a list code * 'a ps_list_code_monoid

module PS_code_list (A:sig type t end) =
struct
  type sta = A.t list
  type t = A.t ps_list_code_monoid
  let rec lift_list tl = function
      [] -> tl
    | h :: t -> .< .~h :: .~(lift_list tl t) >.
  let rec cd : t -> sta code = function
      Empty -> .< [] >.
    | SCons (h, Empty) -> lift_list .<[]>. h
    | DCons (h, Empty) -> h
    | SCons (h, t) -> .< .~(lift_list (cd t) h) >.
    | DCons (d, t) -> .< .~d @ .~(cd t) >.
  let eq x y = match x, y with
    | Empty, Empty -> EQ
    | _ -> UNKNOWN
  let now : t -> sta option = function
      Empty -> Some []
    | _ -> None
  let dyn l = DCons (l, Empty)
  let sta l = SCons (l, Empty)
  let zero = Empty
  let rec (<+>) l r = match l with
      Empty -> r
    | SCons (s, tl) ->
      (match tl <+> r with
       | SCons (s', tl) -> SCons (s @ s', tl)
       | Empty | DCons _ as r -> SCons (s, r))
    | DCons (s, tl) ->
      (match tl <+> r with
       | DCons (s', tl) -> DCons (.<.~s @ .~s'>., tl)
       | Empty | SCons _ as r -> DCons (s, r))
end

type tn' =
  | Dyn : int code -> tn'
  | Max : int option * tn' list -> tn'
  | Add : int option * tn' list -> tn'
type tn = St of int | Dn of tn'

module Tropical_nats =
struct
  type t = tn
  type sta = int

  let now = function
    | St x -> Some x
    | Dn _ -> None
  let eq x y = match now x, now y with
    | Some x, Some y when x = y -> EQ
    | Some x, Some y -> NEQ
    | _ -> UNKNOWN
  let rec cd' = function
    | Dyn x -> x
    | Max ((Some 0 |None), []) -> .<0>.
    | Max ((Some 0 |None), h::t) -> dmaximum (cd' h) t
    | Max (Some x, ds) -> dmaximum .<x>. ds
    | Add (_, Add _ :: _) -> assert false
    | Add ((Some 0 |None), []) -> .<0>.
    | Add ((Some 0 |None), h::t) -> dadd (cd' h) t
    | Add (Some x, ds) -> dadd .<x>. ds
  and dmaximum h t = List.fold_left (fun x y -> .< max .~x .~(cd' y) >.) h t
  and dadd h t = List.fold_left (fun x y -> .< .~x + .~(cd' y) >.) h t
  let cd = function
    | St x -> .< x >.
    | Dn d -> cd' d
  let dyn d = Dn (Dyn d)
  let sta s = St s
  let zero = sta 0

  let add_opts x y = match x, y with
      None, None -> None
    | (None, Some x) | (Some x, None) -> Some x
    | Some x, Some y -> Some (x + y)

  let rec add_sd s d = match d with
    | (Dyn _ | Max _) as d -> Add (Some s, [d])
    | Add (None, ds)       -> assert false(* Add (Some s, ds) *)
    | Add (Some s', ds)    -> assert false (* Add (Some (s + s'), ds) *)

  let rec add_dd l r = match l, r with
      ((Max _ | Dyn _), _)
    | (_, (Max _ | Dyn _)) -> Add (None, [l;r])
    | Add (s1, d1), Add (s2, d2) -> Add (add_opts s1 s2, d1 @ d2)

  let (<+>) l r = match l, r with
      St x, St y -> St (x + y)
    | St x, Dn y
    | Dn y, St x -> Dn (add_sd x y)
    | Dn x, Dn y -> Dn (add_dd x y)

  let max_opts x y = match x, y with
      None, None -> None
    | (None, Some x) | (Some x, None) -> Some x
    | Some x, Some y -> Some (x + y)

  let rec max_sd s d = match d with
    | (Dyn _ | Add _) as d -> Max (Some s, [d])
    | Max (None, ds)       -> Max (Some s, ds)
    | Max (Some s', ds)    -> Max (Some (s + s'), ds)

  let rec max_dd l r = match l, r with
      ((Add _ | Dyn _), _)
    | (_, (Add _ | Dyn _)) -> Max (None, [l;r])
    | Max (s1, d1), Max (s2, d2) -> Max (max_opts s1 s2, d1 @ d2)

  let max l r = match l, r with
      St x, St y -> St (x + y)
    | St x, Dn y
    | Dn y, St x -> Dn (max_sd x y)
    | Dn x, Dn y -> Dn (max_dd x y)

end

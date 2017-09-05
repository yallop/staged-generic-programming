Code from the paper

&nbsp;&nbsp;&nbsp;&nbsp;[**Staged Generic Programming**][staged-generic-programming]  
&nbsp;&nbsp;&nbsp;&nbsp;Jeremy Yallop  
&nbsp;&nbsp;&nbsp;&nbsp;ICFP 2017  

### Installation

```
opam switch 4.02.1+modular-implicits-ber
opam pin add metaocaml-syb https://github.com/yallop/staged-generic-programming.git
```


### Usage

The following examples assume that you have loaded the package and brought the instances into scope:

```ocaml
# #require "metaocaml-syb";;
# open Metasyb_instances_;;
# open Metasyb_schemes_;;
# open Metasyb_instantiate;;
# open Partially_static;;
```


```ocaml
# implicit module P = PS_code_list(struct type t = int end);;

# (generateQps (listify_  (fun x -> .<.~x mod 2 = 0 >.))
    : ((int * bool option) list -> int list) code);;
  - : ((int * bool option) list -> int list) code = .<
let rec r_69 x_70 =
  match x_70 with
  | [] -> []
  | h_104::t_105 ->
      let (x_131,y_132) = h_104 in
      if (x_131 mod 2) = 0 then x_131 :: (r_69 t_105) else r_69 t_105 in
fun x_1  -> r_69 x_1>. 
;;
```

[staged-generic-programming]: https://www.cl.cam.ac.uk/~jdy22/papers/staged-generic-programming.pdf
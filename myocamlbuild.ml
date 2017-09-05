open Ocamlbuild_plugin;;

let () = dispatch begin
    function
    | After_rules ->
      flag ["ocaml"; "link"; "use_metalib"] & A"metalib.cma";
    | _ -> ()
  end

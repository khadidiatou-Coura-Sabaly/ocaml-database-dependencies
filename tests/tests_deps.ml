open Type
open Deps
open Normalisation


let t = {
  cols = [("A", (TInt, false)); ("B", (TInt, false)); ("C", (TInt, false))];
  rows = [
    [VInt 1; VInt 10; VInt 100];
    [VInt 1; VInt 20; VInt 100]; 
    [VInt 2; VInt 10; VInt 200]
  ]
} ;;

let run_deps_tests () =
  Printf.printf "-- TESTS DEPENDANCES --\n";

  (* Test COMPUTE_DEPS *)
  let d = compute_deps t in
  if List.length d > 0 then Printf.printf "\nCompute_deps réussi\n" else Printf.printf "\nCompute_deps pas réussi\n";

  (* Test ELEMENTARY *)
  let ed = compute_elementary_deps t in
  (* On vérifie qu'on n'a pas de RHS multiple *)
  let is_elem = List.for_all (fun (_, rhs) -> List.length rhs = 1) ed in
  if is_elem then Printf.printf "\nElementary_deps réussi\n" else print_endline "\nElementary_deps  réussi\n";

  (* Test NORMALIZATION *)
  let niveau = normalization_level t in
  Printf.printf "\nNiveau trouvé : %d \n" niveau;
  Printf.printf ""
;;
(*test des fonctions insert, prod, projection,restrict*)
open Type
open Table

(*Table de base pour les tests*)
let t1 = {
  cols = [("ID", (TInt, false)); ("Nom", (TText, false))];
  rows = [[VInt 1; VText "Alice"]]
} ;;

let run_ops_tests () =
  Printf.printf "-- DÉBUT DES TESTS DES OPÉRATIONS --\n";

  (* Test INSERT *)
  let t_ins = insert t1 [VInt 2; VText "Bob"] in
  if List.length t_ins.rows = 2 then Printf.printf  "Insert réussi" else Printf.printf "Insert non réussi";

    (* Test PROJECTION *)
  let t_proj = projection t1 ["Nom"] in
  if List.length t_proj.cols = 1  then 
    Printf.printf "Projection réussie" 
  else Printf.printf "Projection non reussi";

  (* Test RESTRICT *)

  (* Test pour restrict : est-ce que le premier élément est l'entier 1 ? *)
  let test_premier_est_un ligne =
       match ligne with
          | VInt 1 :: _ -> true
          | _ -> false
  in


  let t_rest = restrict t1 test_premier_est_un in
  if List.length t_rest.rows = 1 then Printf.printf "Restrict réussi : une ligne sur deux gardée" else Printf.printf "Restrict non réussi";

  Printf.printf ""
  ;; 
open Type
open Utils
open Table
open Deps


(** [test_2nf elementary keys] : fd list -> string list list -> bool
  @requires [elementary] contient des dépendances ou RHS contient une seule colonne, [keys] des clés candidates.
  @ensures retourne [true] si la table satisfait la 2NF.
*)
let test_2nf elementary keys =
   let rec check_deps deps =
    match deps with
    |[] -> true
    |(lhs , [rhs_val])::rest ->
      (* Un attribut de droite est "primaire" s'il appartient à au moins une clé. *)
      let is_rhs_prime = is_prime_attribute rhs_val keys in
      (* Une dépendance est partielle si lhs est un sous-ensemble strict
      d'une clé candidate. Cela signifie que lhs n'utilise pas toute la clé. *)
      let exists_partial = List.exists (fun k -> is_subset lhs k && lhs <> k) keys in
      (* Si l'attribut de droite n'est pas premier et que la dépendance est partielle,
       alors la 2NF est violée. *)
      if not is_rhs_prime && exists_partial then false
      else check_deps rest
    | _ :: rest -> 
       (* On ignore les cas où RHS n'est pas singleton, même si en principe
        [elementary] devrait déjà contenir uniquement des RHS de taille 1. *)
        check_deps rest
    in check_deps elementary
  ;;


(** [test_3nf elementary keys columns] : fd list -> string list list -> string list -> bool
  @requires [columns] est la liste des attributs de la table.
  @ensures retourne [true] si la table satisfait la 3NF.
*)
let test_3nf elementary keys columns =
  let rec check_deps deps =
    match deps with
    | [] -> true
    | (lhs, [rhs_val]) :: rest ->
      (* La 3NF est respectée si : lhs est une clé ou rhs_val est un attribut premier. *)
        if is_key lhs columns elementary || is_prime_attribute rhs_val keys then
          check_deps rest
        else false
    | _ :: rest -> check_deps rest
  in check_deps elementary
;;


(** [test_bcnf elementary keys columns] : fd list -> string list list -> string list -> bool
  @requires [columns] est la liste des attributs de la table.
  @ensures retourne [true] si la table satisfait la BCNF.
*)
let test_bcnf elementary keys columns =
  let rec check_deps deps =
    match deps with
    | [] -> true
    | (lhs, _) :: rest ->
        if is_key lhs columns elementary then check_deps rest
        else false
  in check_deps elementary
;;



(** [normalization_level tbl] : table -> int
  @requires [tbl] est une table valide.
  @ensures retourne 1, 2, 3 ou 4 selon le plus haut niveau de normalisation atteint.
*)
let normalization_level tbl = 
  let columns = get_col_names tbl.cols in
  let elementary = compute_elementary_deps tbl in
  let keys = find_keys columns elementary in
  
  if test_bcnf elementary keys columns then 4
  else if test_3nf elementary keys columns then 3
  else if test_2nf elementary keys then 2
  else 1 (* Par défaut, au moins 1NF *)
;;







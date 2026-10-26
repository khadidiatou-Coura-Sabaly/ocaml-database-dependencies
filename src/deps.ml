open Type
open Utils
open Table


(** [check_dependency_two_rows r1 r2 tbl_cols lhs rhs] : row -> row -> schema -> string list -> string list -> bool
   @requires [r1] et [r2] sont des lignes de même longueur que [tbl_cols].
   @ensures retourne [true] si la dépendance [lhs -> rhs] n'est pas violée par ce couple de lignes.
*)
let check_dependency_two_rows r1 r2 tbl_cols lhs rhs =
  let v_lhs1 = filter_row r1 tbl_cols lhs in
  let v_lhs2 = filter_row r2 tbl_cols lhs in
  if equal_values_list v_lhs1 v_lhs2 then
    let v_rhs1 = filter_row r1 tbl_cols rhs in
    let v_rhs2 = filter_row r2 tbl_cols rhs in
    equal_values_list v_rhs1 v_rhs2
  else true (* Si LHS est différent, la règle n'est pas cassée *)
;;


(** [check_all_pairs r1 rest_rows tbl_cols lhs rhs] : row -> row list -> schema -> string list -> string list -> bool
   @requires [r1] appartient à la table analysée.
   @ensures retourne [true] si [r1] respecte [lhs -> rhs] avec toutes les lignes de [rest_rows].
*)
let rec check_all_pairs r1 rest_rows tbl_cols lhs rhs =
  match rest_rows with
  | [] -> true
  | r2 :: tail ->
      if check_dependency_two_rows r1 r2 tbl_cols lhs rhs then
        check_all_pairs r1 tail tbl_cols lhs rhs
      else false
   ;;


(** [check_table_fd all_rows tbl_cols lhs rhs] : row list -> schema -> string list -> string list -> bool
   @requires [all_rows] est la liste des lignes d'une table.
   @ensures retourne [true] si la dépendance [lhs -> rhs] est vérifiée sur toute la table.
*)

let rec check_table_fd all_rows tbl_cols lhs rhs =
  match all_rows with
  | [] -> true
  | r1 :: rest ->
      if check_all_pairs r1 rest tbl_cols lhs rhs then
        check_table_fd rest tbl_cols lhs rhs
      else false
   ;;


(** [compute_deps tbl] : table -> fd list
   @requires [tbl] est une table valide.
   @ensures retourne toutes les dépendances fonctionnelles observées dans la table.
   @raises Failure si [tbl] n'est pas valide.
*)
let compute_deps tbl =
   let names = get_col_names tbl.cols in
   let possible_sets = powerset names in
   (*enlever la liste vide de possible_sets*)
   let rec enlever_vide l =
      match l with 
      |[] -> []
      |[]::rest -> enlever_vide rest
      |x::rest -> x::enlever_vide rest
   in
   let sets = enlever_vide possible_sets 
   in 

   (*boucle pour les parties droites (RHS)*)
   let rec loop_rhs lhs rhs_list =
      match rhs_list with
      |[] -> []
      |rhs::rest ->
         if check_table_fd tbl.rows tbl.cols lhs rhs then
            (lhs, rhs)::(loop_rhs lhs rest)
         else loop_rhs lhs rest ;
      in 
      (*boucle sur les parties gauches (LHS)*)
      let rec loop_lhs lhs_list = 
         match lhs_list with
         |[] -> []
         |lhs::rest ->
            (loop_rhs lhs sets)@(loop_lhs rest)
         in
         loop_lhs sets ;;


(** [one_column_rhs all_dep] : fd list -> fd list
   @requires [all_dep] est une liste de dépendances fonctionnelles.
   @ensures transforme les RHS pour qu'elles contiennent une seule colonne.
*)
let rec one_column_rhs all_dep = 
   match all_dep with
   |[] -> []
   |(lhs,rhs)::rest ->
      (* Pour chaque nom dans [rhs], on crée une dépendance [lhs -> [name]]. *)
      let rec make_singletons r = 
         match r with
         |[] -> []
         |name::r_rest ->
            if List.mem name lhs then make_singletons r_rest (*RHS et LHS ne doivent pas avoirs une intersection non vide pour une dependansce elementaire*)
            else (lhs , [name])::(make_singletons r_rest)
         in
         (make_singletons rhs)@(one_column_rhs rest)
      ;;


(** [is_minimal lhs rhs all_deps] : string list -> string list -> fd list -> bool
   @requires [lhs] et [rhs] décrivent une dépendance tel que [rhs] ne contienne qu'une seule colonne.
   @ensures retourne [true] si [lhs -> rhs] est minimale parmi [all_deps].
*)
let is_minimal lhs rhs all_deps = 
   (*on cherche si un autre lhs est inclus dans notre lhs pour le meme rhs*)
   let rec check deps = 
      match deps with
      |[] -> true
      |(lhs_prime, rhs_prime)::rest ->
         if rhs = rhs_prime && lhs <> lhs_prime && is_subset lhs_prime lhs then
            false
         else
            check rest

         in check all_deps 
      ;;


(** [compute_elementary_deps tbl] : table -> fd list
   @requires [tbl] est une table valide.
   @ensures retourne les dépendances élémentaires : RHS contient une seule colonne et LHS minimale.
   @raises Failure si [tbl] n'est pas valide .e
*)
let compute_elementary_deps tbl = 
   let all_deps = compute_deps tbl in
   let single_rhs_deps = one_column_rhs all_deps in
   let rec filter_minimal d_list = 
      match d_list with 
      |[] -> []
      |(lhs,rhs)::rest ->
         if is_minimal lhs rhs single_rhs_deps then
            (lhs,rhs)::(filter_minimal rest)
         else
            filter_minimal rest
   in filter_minimal single_rhs_deps 

;;


(** [determines lhs target elementary_deps] : string list -> string -> fd list -> bool
   @requires [elementary_deps] contient des dépendances avec  RHS contenant une seule colonne.
   @ensures retourne [true] si [lhs] détermine directement [target].
*)
let rec determines lhs target elementary_deps = 
   match elementary_deps with
   |[] -> false
   |(l,[r])::rest ->
      if l = lhs && r=target then true
      else determines lhs target rest
   |_::rest -> determines lhs target rest
   ;;


(** [is_key lhs cols elementary_deps] : string list -> string list -> fd list -> bool
   @requires [cols] est la liste des colonnes de la table.
   @ensures retourne [true] si [lhs] détermine toutes les colonnes de [cols].
*)
let is_key lhs cols elementary_deps = 
   let rec check_all cols = 
      match cols with
      |[] -> true 
      |c::rest ->
         if List.mem c lhs || determines lhs c elementary_deps then check_all rest
         else false
      in check_all cols
   ;;


(** [find_keys columns elementary_deps] : string list -> fd list -> string list list
   @requires [columns] est la liste des attributs de la table.
   @ensures retourne les combinaisons de colonnes qui sont des clés selon [elementary_deps].
*)
let find_keys columns elementary_deps = 
   let all_combos = powerset columns in
   let rec filter_keys combos = 
      match combos with 
      |[] -> []
      |c::rest ->
         if c<> [] && is_key c columns elementary_deps then
            (*on verifie la minimilate pour que ce soit une cle candidate*)
            c::filter_keys rest
         else filter_keys rest 
      in filter_keys all_combos
   ;;


(** [is_prime_attribute col keys] : string -> string list list -> bool
   @requires [keys] contient une liste de clés candidates.
   @ensures retourne [true] si [col] appartient à au moins une clé candidate.
*)
let is_prime_attribute col keys =
   let rec check k_list = 
      match k_list with
      |[] -> false
      |k::rest -> if List.mem col k then true else check rest
   in check keys
;;

open Type
open Utils


(** [check_rows_length rows n] : row list -> int -> bool
   @requires [n] est un entier positif ou nul.
   @ensures retourne [true] si toutes les listes de [rows] ont exactement [n] valeurs.
*)
let rec check_rows_length rows n  =
      match rows with 
      | [] -> true
      | r::rest -> 
         (* On vérifie la longueur de chaque ligne une par une. *)
         if List.length r = n then check_rows_length rest n
      else false 
   ;;


(** [check_row_values row cols] : row -> schema -> bool
   @requires [row] et [cols] ont la même longueur.
   @ensures retourne [true] si les valeurs de [row] respectent les types de [cols].
*)
let rec check_row_values row col = 
   match row, col with 
   | [] , [] -> true
   | v::rrest , (_ , (t, nullable))::crest -> 
      let ok =
      match v with 
      |VNull -> nullable
      |VInt _  -> t=TInt
      |VText _ -> t = TText
      in 
      if ok then
         check_row_values rrest crest
      else false
   | _ -> false 
   ;;


(** [combine r1 rows] : row -> row list -> row list
   @requires [r1] est une ligne valide.
   @ensures concatène [r1] avec chaque ligne de [rows].
   Cette fonction sert de base au produit cartésien.
*) 
 let rec combine r1 rows = 
   match rows with 
   | [] -> []
   | r2::rest ->
      (* On concatène la ligne de gauche avec chaque ligne de droite. *)
      (r1 @ r2):: combine r1 rest 
   ;;


(** [check_table tbl] : table -> bool
   @requires [tbl] contient un schéma et des lignes candidats à validation.
   @ensures retourne [true] si la table est bien formée.
*)
let check_table tbl =
  let n = List.length tbl.cols in 
   if not (check_rows_length tbl.rows n ) then 
      false
   else
      let rec check_all_rows rows = 
         match rows with
         | [] -> true
         | r::rest -> 
            (* Chaque ligne doit respecter exactement le schéma. *)
            if check_row_values r tbl.cols then
            check_all_rows rest
         else
            false
         in check_all_rows tbl.rows

;;


(** [insert tbl row] : table -> row -> table
   @requires [tbl] est une table valide et [row] a la bonne longueur.
   @ensures retourne une nouvelle table contenant [row] en tête des lignes.
   @raises  Failure si la table est invalide , si la longueur est incorrecte
    ou si les types de [row] ne correspondent pas au schema .
*)
let insert tbl row = 
   if  not (check_table tbl) then failwith "Tableau invalide"
   else if List.length row <> List.length tbl.cols then failwith " Longueur incorrecte"
   else if not (check_row_values row tbl.cols) then failwith "Types incorrectes"
   else {tbl with rows = row::tbl.rows}
;;

(** [combine r1 rows] : row -> row list -> row list
   @requires [r1] est une ligne valide.
   @ensures concatène [r1] avec chaque ligne de [rows].
    Cette fonction sert de base au produit cartésien.
*)
 let rec combine r1 rows = 
   match rows with 
   | [] -> []
   | r2::rest ->
      (r1 @ r2):: combine r1 rest 
   ;;


(** [prod tbl1 tbl2] : table -> table -> table
   @requires [tbl1] et [tbl2] sont des tables valides.
   @ensures retourne le produit cartésien des deux tables.
   @raises Failure si l'une des deux tables est invalide.
*)                  
let prod tbl1 tbl2 = 
   if (not ( check_table tbl1)) || (not ( check_table tbl2)) then failwith "une (chacune) des Tables est (sont) invalide(s)" 
   else 
      let new_cols = tbl1.cols @ tbl2.cols in
         let rec produit rows1 = 
            match rows1 with
            | []->[]
            | r1 :: rest -> combine r1 tbl2.rows @ produit rest
            in 
            {cols = new_cols ; rows = produit tbl1.rows}
;;


(** [projection tbl fields] : table -> string list -> table
   @requires [tbl] est valide et tous les noms de [fields] existent dans [tbl].
   @ensures retourne la table projetée sur les colonnes demandées.
   @raises Failure si la table est invalide , si une colonne demandée
   n'existe pas , ou si la projection ne peut pas être effectuée.
*)
let projection tbl fields = 
   if not (check_table tbl) then failwith "Table invalide"
   else if not (all_present tbl.cols fields) then failwith "Projection impossible"
   else if tbl.cols = [] || fields = [] then failwith "Projection impossible"
   else 
      let rec filter_all_rows rows cols fields =
         match rows with
         |[] -> []
         |r::rrest -> 
            (* Chaque ligne est filtrée selon les colonnes demandées. *)
            (filter_row r cols fields)::(filter_all_rows rrest cols fields)

      in
      {cols = filter_cols tbl.cols fields ;
      rows = filter_all_rows tbl.rows tbl.cols fields
      }
      
;;


(** [restrict tbl test] : table -> (row -> bool) -> table
   @requires [test] est un prédicat applicable à chaque ligne de [tbl].
   @ensures retourne une table ne contenant que les lignes vérifiant [test].
*)
let restrict tbl f = 
   let rec filter rows = 
      match rows with 
      |[] -> []
      |r::rest ->
         (* On conserve uniquement les lignes qui satisfont le prédicat. *)
         if f r then r::filter rest
         else 
            filter rest
         in {tbl with rows = filter tbl.rows }
  
;;
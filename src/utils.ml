
open Type


(** [get_col_names cols] : schema -> string list
   @requires [cols] est un schéma bien formé.
   @ensures retourne la liste des noms des colonnes de [cols], dans le même ordre.
*)
let rec get_col_names cols =
   match cols with
   |[] -> []
   |(name,_)::crest -> 
      (* On conserve le nom de la colonne courante, puis on traite le reste. *)
      name::(get_col_names crest)
   ;;


(** [all_present cols fields] : schema -> string list -> bool
   @requires [cols] est un schéma valide et [fields] est une liste de noms.
   @ensures retourne [true] si tous les noms de [fields] apparaissent dans [cols].
*) 
let rec all_present cols fields = 
   let  l1 = get_col_names cols
   in
   match fields with 
   |[] -> true
   |name::nrest-> 
      (* On vérifie que la colonne courante demandée appartient bien au schéma,
      puis on continue avec le reste des noms. *)
      if (List.mem name l1) then all_present cols nrest
      else false
;;


(** [filter_row row cols fields] : row -> schema -> string list -> dbvalue list
   @requires [row] et [cols] ont la même longueur, et [fields] contient des noms de colonnes.
   @ensures retourne les valeurs de [row] correspondant aux colonnes de [fields].
*)
let rec filter_row row cols fields =
   match row,cols with
   |[],_ ->[]
   |_,[] ->[]
   |r::rrest,(name,_)::crest-> 
      (* Si le nom de la colonne courante appartient aux colonnes demandées,
         on conserve la valeur correspondante dans la ligne filtrée. *)
      if List.mem name fields then 
         r:: (filter_row rrest crest fields)
   else
      filter_row rrest crest fields
    ;;


(** [filter_cols cols fields] : schema -> string list -> schema
   @requires [cols] est un schéma valide.
   @ensures retourne le sous-schéma contenant uniquement les colonnes de [fields].
*)
let  rec filter_cols cols fields = 
   match cols with
   |[] -> []
   | (name , ct)::crest ->
      (* Même principe que pour [filter_row] : on ne garde que les colonnes
         qui apparaissent dans la liste demandée. *)
      if List.mem name fields then 
         (name,ct)::(filter_cols crest fields)
    else
      filter_cols crest fields 
   ;;


(** [equal_value v1 v2] : dbvalue -> dbvalue -> bool
   @requires [v1] et [v2] sont des valeurs dbvalue bien formées.
   @ensures retourne [true] si [v1] et [v2] représentent la même valeur.
*)
let equal_value v1 v2 = 
   match v1,v2 with
   |VInt a, VInt b -> a=b
   |VText a , VText b -> a=b
   |VNull,VNull -> true
   |_ -> false
;;


(** [powerset l] : 'a list -> 'a list list
   @requires [l] est une liste finie.
   @ensures retourne l'ensemble des sous-listes possibles de [l].
*)
let rec powerset l =
  match l with
  | [] -> [[]]
  | x :: xs ->
      let ps = powerset xs in
      (* Pour chaque sous-ensemble du reste, on construit une version
         sans [x] et une version avec [x]. *)
      ps @ List.map (fun ss -> x :: ss) ps
   ;;


(** [equal_values_list l1 l2] : dbvalue list -> dbvalue list -> bool
   @requires [l1] et [l2] ont la même longueur.
   @ensures retourne [true] si les deux listes sont égales terme à terme.
*)
let rec equal_values_list l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | v1::r1, v2::r2 -> 
     (* On compare les têtes, puis récursivement le reste des listes. *)
      if equal_value v1 v2 then equal_values_list r1 r2 
      else false
  | _ -> false
;;

(** [is_subset l1 l2] : 'a list -> 'a list -> bool
   @requires [l1] et [l2] sont des listes finies.
   @ensures retourne [true] si tous les éléments de [l1] appartiennent à [l2].
*)
let rec is_subset l1 l2 = 
   match l1 with
   |[] -> true
   |x::rest -> 
         (* Tous les éléments de [l1] doivent appartenir à [l2]. *)
      if List.mem x l2 then is_subset rest l2 else false
;;
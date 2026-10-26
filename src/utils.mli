open Type

(** [get_col_names cols] retourne la liste des noms des colonnes du schéma [cols]. *)
val get_col_names : schema -> string list

(** [filter_row row cols fields] extrait dans [row] les valeurs des colonnes
    dont le nom appartient à [fields], en suivant le schéma [cols]. *)
val filter_row : row -> schema -> string list -> dbvalue list

(** [filter_cols cols fields] retourne le sous-schéma de [cols] ne contenant
    que les colonnes dont le nom appartient à [fields]. *)
val filter_cols : schema -> string list -> schema

(** [all_present cols fields] vérifie que tous les noms de [fields]
    sont présents dans le schéma [cols]. *)
val all_present : schema -> string list -> bool

(** [equal_value v1 v2] teste l'égalité entre deux valeurs de base. *)
val equal_value : dbvalue -> dbvalue -> bool

(** [equal_values_list l1 l2] teste l'égalité terme à terme de deux listes de valeurs. *)
val equal_values_list : dbvalue list -> dbvalue list -> bool

(** [powerset l] retourne l'ensemble des sous-listes possibles de [l]. *)
val powerset : 'a list -> 'a list list

(** [is_subset l1 l2] vérifie que [l1] est inclus dans [l2]. *)
val is_subset : 'a list -> 'a list -> bool
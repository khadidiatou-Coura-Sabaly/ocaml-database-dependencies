open Type
open Utils

(** [check_rows_length rows n] vérifie que chaque ligne de [rows]
    contient exactement [n] valeurs. *)
val check_rows_length : row list -> int -> bool

(** [check_row_values row cols] vérifie que les valeurs de [row]
    sont compatibles avec les types du schéma [cols]. *)
val check_row_values : row -> schema -> bool

(** [check_table tbl] vérifie que la table [tbl] est valide :
    longueurs des lignes cohérentes et types respectés. *)
val check_table : table -> bool

(** [insert tbl row] ajoute [row] en tête des lignes de [tbl]
    si la ligne respecte le schéma de la table.
    Lève une exception en cas d'erreur. *)
val insert : table -> row -> table

(** [combine r1 rows] concatène [r1] avec chaque ligne de [rows]. *)
val combine : row -> row list -> row list

(** [prod tbl1 tbl2] retourne le produit cartésien des tables [tbl1] et [tbl2].
    Lève une exception si l'une des tables est invalide. *)
val prod : table -> table -> table

(** [projection tbl fields] retourne la projection de [tbl]
    sur les colonnes listées dans [fields].
    Lève une exception si la projection est impossible. *)
val projection : table -> string list -> table

(** [restrict tbl f] retourne la table obtenue en ne gardant
    que les lignes [r] de [tbl] telles que [f r] vaut [true]. *)
val restrict : table -> (row -> bool) -> table
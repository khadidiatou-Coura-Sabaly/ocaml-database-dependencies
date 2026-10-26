open Type
open Utils
open Table

(** [check_dependency_two_rows r1 r2 cols lhs rhs] vérifie que
    si [r1] et [r2] coïncident sur [lhs], alors elles coïncident aussi sur [rhs]. *)
val check_dependency_two_rows :
  row -> row -> schema -> string list -> string list -> bool

(** [check_all_pairs r1 rows cols lhs rhs] compare [r1] à toutes les lignes de [rows]
    pour vérifier la dépendance fonctionnelle [lhs -> rhs]. *)
val check_all_pairs :
  row -> row list -> schema -> string list -> string list -> bool

(** [check_table_fd rows cols lhs rhs] vérifie que la dépendance fonctionnelle
    [lhs -> rhs] est satisfaite dans toutes les lignes de la table. *)
val check_table_fd :
  row list -> schema -> string list -> string list -> bool

(** [compute_deps tbl] retourne toutes les dépendances fonctionnelles
    vérifiées par les données de la table [tbl]. *)
val compute_deps : table -> fd list

(** [one_column_rhs deps] transforme une liste de dépendances en dépendances
    dont la partie droite contient une seule colonne. *)
val one_column_rhs : fd list -> fd list

(** [is_minimal lhs rhs deps] vérifie que [lhs -> rhs] est minimale parmi [deps]. *)
val is_minimal : string list -> string list -> fd list -> bool

(** [compute_elementary_deps tbl] retourne les dépendances fonctionnelles élémentaires
    extraites de la table [tbl]. *)
val compute_elementary_deps : table -> fd list

(** [determines lhs target deps] vérifie si [lhs] détermine la colonne [target]
    dans la liste de dépendances élémentaires [deps]. *)
val determines : string list -> string -> fd list -> bool

(** [is_key lhs cols deps] vérifie si [lhs] est une clé pour l'ensemble
    des colonnes [cols] selon les dépendances [deps]. *)
val is_key : string list -> string list -> fd list -> bool

(** [find_keys columns deps] retourne les clés candidates parmi les combinaisons
    de [columns], selon les dépendances [deps]. *)
val find_keys : string list -> fd list -> string list list

(** [is_prime_attribute col keys] vérifie si [col] appartient à au moins
    une clé candidate de [keys]. *)
val is_prime_attribute : string -> string list list -> bool
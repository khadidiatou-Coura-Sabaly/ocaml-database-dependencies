open Type
open Utils
open Table
open Deps

(** [test_2nf elementary keys] vérifie si les dépendances élémentaires [elementary]
    respectent la deuxième forme normale vis-à-vis des clés [keys]. *)
val test_2nf : fd list -> string list list -> bool

(** [test_3nf elementary keys columns] vérifie si les dépendances élémentaires [elementary]
    respectent la troisième forme normale pour les colonnes [columns]. *)
val test_3nf : fd list -> string list list -> string list -> bool

(** [test_bcnf elementary keys columns] vérifie si les dépendances élémentaires [elementary]
    respectent la forme normale de Boyce-Codd pour les colonnes [columns]. *)
val test_bcnf : fd list -> string list list -> string list -> bool

(** [normalization_level tbl] retourne le niveau de normalisation de [tbl] :
    1, 2, 3 ou 4 selon le plus haut niveau satisfait. *)
val normalization_level : table -> int
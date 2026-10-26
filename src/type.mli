(** Types primitifs manipulés en base de données. *)
type dbtype =
  | TInt
  | TText

(** Type d'une colonne : type de base et booléen indiquant si NULL est autorisé. *)
type coltype = dbtype * bool

(** Valeurs manipulées dans les lignes des tables. *)
type dbvalue =
  | VInt of int
  | VText of string
  | VNull

(** Schéma d'une table : nom de colonne et type associé. *)
type schema = (string * coltype) list

(** Une ligne d'une table. *)
type row = dbvalue list

(** Une table : schéma et lignes. *)
type table = { cols : schema; rows : row list }

(** Dépendance fonctionnelle lhs -> rhs. *)
type fd = (string list) * (string list)
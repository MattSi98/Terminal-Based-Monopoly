(** This module is resposible for actually beginning the game *)

(** [get_names ()] is a list of names that players entered in the order that they
    entered them *)
val get_names : unit -> string list

(** [play names] plays the game with the players named according to [names]*)
val play : string list -> unit
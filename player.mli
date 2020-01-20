(** The player module contains functions for manipulating and accessing player
    data. *)

(** the type representing a player *)
type t

(** [make id name] makes player with id [id] and name [name] for the 
    beginning of the game IN AN
    INITIAL STATE.  *)
val make : int -> string -> t


(** [get_id player] is the id of [player] *)
val get_id : t -> int

(** [get_name player] returns the name of the player [player]. *)
val get_name : t -> string

(** [get_name_opt player] returns the name of the player option [player]. *)
val get_name_opt : t option -> string

(** [get_money player] returns the money of the player [player]. *)
val get_money : t -> int

(**[get_loc player] returns the player's current location. This location 
   is the index of the property *)
val get_loc : t -> int 

(**[get_bankrupt player] returns the player's bankruptcy status. *)
val get_bankrupt : t -> bool

(** [declare_bankruptcy player] returns a player who is bankrupt and there location
    is set to a negative number. *)
val declare_bankruptcy : t -> t

(** [move_player player n] returns a player who has moved n spaces forward. *)
val move_player : t -> int -> t

(** [give_money amt player] is a [player] with [amt] more money.
    Requires: [amt] is positive. *)
val give_money : int -> t -> t

(** [take_money amt player] is a player which has [player]’s money 
    reduced by [amt] Requires: [amt] is positive and [player]’s money is 
    greater than or equal to [amt] *)
val take_money : int -> t -> t

(** [can_pay_cash player amt] returns true if [player] can pay [amt] in cash, 
    false otherwise*)
val can_pay_cash: t -> int -> bool 

(** [send_to_jail player] is [player] who is now in the jail position*)
val send_to_jail: t -> t

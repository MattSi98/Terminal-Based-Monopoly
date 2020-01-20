(** The consts module holds key data that is important to the game settings. *)

(** This is the numebr of players you want to play with. You can change this*)
val num_players : int

(** This is the number of spaces on a standard monopoly board. DO NOT CHANGE THIS
    OR THE BOARD WON'T BE TRAVERSED CORRECLTY. THE BOARD IS HARD CODED IN AS IT IS A 
    STANDARD  *)
val num_spaces : int

(**The amount of money each player starts with *)
val starting_money : int

(** The amount of money each player recieves for passing go *)
val go_money : int

(** The amount of money a player gets taxed for landing on one of the two
    tax spaces *)
val tax_money : int

(** The amount of money a player is fined when they are sent to jail *)
val jail_money : int

(** The index of the jail property *)
val jail_pos : int
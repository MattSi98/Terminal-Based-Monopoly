(** This module handles operations that consider a single property in isolation.
    the properties in this module have no way of knowing what is going on with other
    properties. Any opertaion that would require that is handled in the Properties
    module. The types are all public so that pattern matching can be done outside
    of this module to make the code easier to follow *)

(** The type representing the different possible colors of a colored property *)
type color = 
  | Purple 
  | LightBlue 
  | Pink 
  | Orange 
  | Red 
  | Yellow 
  | Green 
  | DarkBlue 


(** The type representing a colored property's info *)
type colored_prop_info = 
  {name : string; price : int; is_mortgaged : bool; owner : int option; 
   num_houses : int; rent_prices : int list; build_costs : int; color : color}

(** The type representing a rail road property's info *)
type rr_prop_info = {name : string; price : int; is_mortgaged : bool;
                     owner : int option; rent_prices : int list}

(** The type representing a utility property's info *)
type utility_prop_info = {name : string; price : int; is_mortgaged : bool;
                          owner : int option; rent_multipliers : int list}


(** The type representing all the commerical properties *)
type commercial_type = 
  | Colored of colored_prop_info
  | Railroad of rr_prop_info
  | Utility of utility_prop_info


(** The type representing all the non commercial properties *)
type non_commercial_type = 
  | Go of string * int (* carries how much money one recieves for passing go *)
  | Tax of string * int (* carries how much money one needs to pay on the tax space *)
  | GoJail of string * int (* carries the jail fine *)
  | Jail of string 
  | CCC of string
  | FreeParking of string

(** The type representing a property *)
type t = 
  | Commercial of commercial_type
  | NonCommercial of non_commercial_type

(* These functions can technically be all handled outside of the module due to 
   the pulic nature of the types, but they are here for convenince and organization*)


(** [set_owner player_index property] is the property [property] with its
    owner set to Some [player_index]. 
    Requires: [property] is a commercial property 
    Raises: Failure if precondition is violated*)
val set_owner : int -> t -> t

(** [get_color property] is the color of [property]. 
    Requires: [property] is a colored property 
    Raises: Failure if precondition is violated*)
val get_color : t -> color

(** [get_owner property] is Some id if the player with id owns [property]
    or is None if the Bank owns [property]. 
    Requires: [property] is a commercial property
    Raises: Failure if precondition is violated *)
val get_owner : t -> int option

(** [is_commercial property] is true if [property] is commercial, false otherwise *)
val is_commercial: t -> bool 

(** [is_colored property] is true if [property] is colored, false otherwise *)
val is_colored: t -> bool 

(** [property_is_mortgaged property] is true if property is mortgaged, false otherwise
    Requires: [property] is a commercial property
    Raises: Failure if precondition is violated *)
val property_is_mortgaged: t -> bool 

(** [set_mortgaged property] is [property] mortgaged, only intended for use
    by State
    Requires : [property] is a commercial property
    [property] is not already mortgaged
    [property] has zero houses if colored
    * if colored all the properties of the same color also have zero houses
    Raises: Failure if precondition is violated, * condition not checked*)
val set_mortgaged: t -> t 

(** [set_umortgaged property] is [property] unmortgaged, only intended for use
    by State 
    Requires: [property] is a commercial property
    [property] is mortgaged
    Raises: Failure if precondition violated*)
val set_unmortgaged: t -> t

(** [reset_property property] is [property] reset. Meaning it is not mortgaged, 
    it is owned by the bank, and number of houses is zero if it was colored
    Requires: [property] is commercial
    Raises: Failure if precondition is violated*)
val reset_property : t -> t 

(** [buy_houses num property] is [property] with num_houses increased by [num]
    Requires: [property] is a colored property, num + num_houses [property] is < 5
    Raises: Failure if precondition is violated*)
val buy_houses : int -> t -> t

(** [get_price property] is the price of [property] 
    Requires : [property] is a commercial property
    Raises: Failure if precondition is violated*)
val get_price : t -> int

(** [get_name property] is the name of [property]*)
val get_name: t -> string

(** [sell_houses num property] is [property] with [num] less houses
    Requires: [property] has >= [num] houses
    [property] is a colored property
    [property] is not mortgaged
    Raises: Failure if precondition violated*)
val sell_houses: int -> t -> t

(** [get_houses property] is the number of houses on [property]
    Requires: [property] is a Colored property 
    Raises: Failure if precondition violated*)
val get_houses: t -> int

(*Standard monoply properties in an inital game state*)
val go : t
val mediterranean_ave : t
val baltic_ave : t
val reading_railroad : t
val oriental_ave : t
val vermont_ave : t
val connecticut_ave : t
val st_charles : t
val states_ave : t
val virginia_ave : t
val electric_comp : t
val penn_railroad : t 
val st_james : t
val tenn_ave : t
val ny_ave : t
val kn_ave : t
val ind_ave : t
val ill_ave : t
val bo_railroad : t
val atl_ave : t
val ventnor_ave : t
val marv_gardens : t
val water_works : t
val pacific_ave : t
val ncar_ave : t
val penn_ave : t
val sl_railroad : t
val park_place : t
val boardwalk : t
val comm_chest : t
val chance : t
val go_jail : t
val jail : t 
val free_parking : t
val tax : t
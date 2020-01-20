(** This module handles operations that consider groups of properties and
    their relationships to eachother. There are several invariants that must hold
    1.) For all colored properties, p, if num_houses p > 0 then owner p = owner p',
    the owner is not the Bank, and p' is not mortgaged
    where p' is any property that has the same color as p 
    2.) For all commercial properties, p, if p is owned by the bank then 
    the property is not mortgaged and has zero houses if colored.*)




open Property
(** [check_invs properties] is [properties] if [properties] satisfies all the 
    invariants. Otherwise crashes the program with helpful error msg*)
val check_invs: t list -> t list


(** [make_properties_list ()] is a list of properties in the order of a 
    standard monopoly board in th initial state*)
val make_properties_list : unit -> t list

(** [no_houses_on_color property properties] is true if property is uncolored
    or if all the properties that have the same color as [property] don't have any
    houses on them *)
val no_houses_on_color : t -> t list -> bool 

(** [get_player_properties player_index properties] is a list of properties that
    are owned by the player at [player_index] *)
val get_player_properties : int -> t list -> t list


(**  [can_buy_house player_index properties property] is true if player at [player_index]
     executively owns [property], none of the properties of the same color are mortgaged,
     and property does not already have 5 houses. False otherwise. Note this takes no account of how
     much money or assets the player at [player_index] has. Hence this could return true
     even if the player at [player_index] can't afford to buy a house at the moment.
     That is a seperate check.
     Requires: [property] is a colored property
     Raises: Failure if precondition is violated*)
val can_buy_house : int -> t list -> t -> bool


(** [value_of_assets player_index properties] is the amount of money 
    that the player at [player_index] would get if they sold all their houses and mortgaged
    all their unmortgaged properties. Properties that are already mortgaged do not count
    to this total*)
val value_of_assets : int -> t list -> int


(** [can_mortgage player_index properties property] is true if player at [player_index] 
    can mortgage the property 
    [property] given what is going on in [properties]. Considerations have to be 
    made regarding if the player owns the property, if it is already 
    mortgaged, if it has any houses on it, if any of
    the properties of the same color have houses on them. False otherwise. 
    Requires: [property] is a commercial property
    Raises: Failure if precondition is violated*)
val can_mortgage : int -> t list -> t -> bool



(** [commercial_rent properties roll property] is the amount of money one 
    would have to pay when landing on [property] given that they don't own it, 
    and another player does
    Requires: [property] is a commercial property
            [property] is owned by a player not the Bank
    Raises: Failure if precondition is violated *)
val commercial_rent : t list -> int -> t -> int


(** [get_property_index property properties] is the index of [property] in [properties] 
    Requires: [property] is in [properties]
    Raises: Failure if precondition is violated*)
val get_property_index : t -> t list -> int
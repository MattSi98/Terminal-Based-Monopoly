(** This module is where the game state is managed and mainpulated. Commands
    rely on the functionality in this module*)

(** represents the state of the game. *)
type t

(** Represents the scenarios that could happen when a player is moved to the 
    [Landing] phase.

    A [Landing] phase is entered after rolling.

    The [Landing Buy property] phase is entered when the current player 
    can choose to buy
    the property they are on. This is the case when the property is commercial
    and is currently owned by the Bank

    The [Landing Pay (property, Some player, int)] phase is entered when the current 
    player must pay another player or the bank (the player recieving the money is 
    [Some player]). This is the case when either the current player lands 
    on an unmortgaged, 
    commercial property, owned by someone other than current player, or if they land 
    on a tax space or go to jail space.

    The [Landing Nil property] phase is entered when the current player lands on a space
    where there is nothing for them to choose to do. This is the case when they land
    on a commercial property that is mortgaged, a commercial property that they own,
    or a non commercial property other than tax or go to jail.
*)
type scenario =
  | Buy of Property.t
  | Pay of (Property.t * Player.t option * int)
  | Nil of Property.t

(** Represents the phase of the current turn 

    In the [Rolling] phase a player can make trades, buy/sell houses, 
    mortgage/unmortgage properties, make status checks, and roll. 
    Rolling triggers the [Landing] phase. Starting a trade triggers
    a [Trade Roll] phase on the same turn 

    In the [Landing Buy] phase a player can sell houses, mortgage properties, 
     make status checks, buy, or pass. Buying triggers the 
    [Final] phase. Passing triggers the [Auction] phase

    In the [Landing Pay] phase a player can sell houses, mortgage properties, 
    make status checks, pay, or declare bankruptcy. Paying triggers the 
    [Final] phase. Declaring bankruptcy triggers the [Roll] phase of the next player.
    This player will never get a turn again. In the special case where they pay on
    the Go to jail property the [Final] phase is still triggered, but the player
    is teleported to jail property. They do not recieve money for passing go. 

    In the [Landing Nil] phase a player does not issue any commands. Anything 
    needed is done on the backend and then the [Final] phase is automatically
    triggered

    In the [Auction property] phase a player does not issue any commands. An auction
    is automatically triggered by passing in [Landing Buy] and then the [Final]
    phase is triggered

    In the [Trade (prev_phase, p2)] phase a player can trade x for y or cancel.
    Cacelling triggers prev_phase on the same turn. Trading trades and stays in 
    the same phase. Combinations of (x,y) can be (cash, property), (property, cash),
    (property, property) otherwise the command is invalid. Players can also 
    run check properties and check money

    In the [Final] phase a player can make trades, buy/sell houses, 
    mortgage/unmortgage properties, make status checks, and end turn. 
    Ending turn triggers the [Roll] phase of the next player. Starting a trade
    triggers the [Trade Final] phase on the same turn
*)
type phase =
  | Rolling
  | Landing of scenario
  | Auction of Property.t
  | Trading of (phase*Player.t)
  | Final

(** [make_init_state names] returns an initial game state with the player in [names]
    and starts at the [Roll] phase of the first player*)
val make_init_state : string list -> t

(** [get_player state n] returns the [n]th player in state [state]
    Example: get_player my_state 0 = 0th player of the game*)
val get_player : t -> int -> Player.t

(** [get_current_player state] returns the player whos turn it is in state [state] *)
val get_current_player : t -> Player.t

(** [get_turn state] is the turn number in the state [state] *)
val get_turn : t -> int

(** [move_player state player num] is a state with the same turn, but player [player]
    has been moved forward [num] and the phase of [state] has been set to the proper
    Landing phase based on the scenario. *)
val move_player : t -> Player.t -> int -> t

(** [next_turn state] is a state with the turn set to the next player who is
    not bankrupt and the phase is back to [Roll].
    Requires: [state] is not complete
    Raises: Failure if precondition violated*)
val next_turn : t -> t

(** [get_phase state] returns the phase of the state [state]*)
val get_phase : t -> phase

(** [get_players_properties state player] is a list of the properties owned by player
    [player] in state [state] *)
val get_players_properties : t -> Player.t -> Property.t list

(** [give_money state player amount] returns a state where [player] has [amount]
    more money from the inital state [state]. 
    Requires: [amt] is positive
    Raises: Failure if precondition is violated*)
val give_money : t -> Player.t -> int -> t

(** [get_location state player] is the property that [player] is on in
    state [state]*)
val get_location : t -> Player.t -> Property.t

(** [get_all_players state] returns the list of players as a player.t list*)
val get_all_players : t -> Player.t list

(** [set_property_owner state property player] returns the state with [player] 
    set as the owner of the property that corresponds to the property name [property_name]. 
    Requires : [property] is commercial
    Raises: Failure if precondition is violated*)
val set_property_owner: t -> Property.t  -> Player.t -> t

(** [is_complete state] is true if the game is done false otherwise, does not
    actually do any checking of win conditions, complete has to be set before the
    function is called*)
val is_complete : t -> bool

(** [get_winner_name state] is the name of the winner. 
    Requires: is_complete [state] is true
    Raises: Failure if precondition is violated*)
val get_winner_name : t -> string

(** [take_money player amt state] is the state with that player’s money 
    field set to its old money minus [amt]. 
    Reqiures: [player] money greater than or equal to [amt] and [amt] is positive 
    Raises: Failure if precondition is violated*)
val take_money : Player.t -> int -> t -> t

(**[can_pay_total state player amt] returns true if money + assets >= amt for 
   [player]'s money and assets*)
val can_pay_total: t -> Player.t -> int -> bool


(**[set_player_bankrupt state player] is the state in which [player] is bankrupt. 
   and all the properties that [player] owned are now unmortgaged, have zero houses, and
   are owned by the Bank. If there is only one player left who is not bankrupt
   in this new state then this state is also set to complete. The turn is also set
   to the next non-bankrupt player and the phase is set to Roll
   Requires: [player] is not already bankrupt and [state] is not complete
   Raises: Failure if precondition violated*)
val set_player_bankrupt : t -> Player.t -> t


(** [buy_houses state player property num] is the state where [player] has 
    bought [num] houses on [property]. Taking money from the player for the payment
    is handled in the function
    Requires: [player] can pay cash for [num] houses on [property],
            [property] is a colored property
            [player] executively owns [property] and no property of the 
            same color is mortgaged 
            [num] + num_houses property is < 5
    Raises: Failure if precondtions are violated*)
val buy_houses : t -> Player.t -> Property.t -> int -> t

(** [sell_houses state player property num] is the state where [player] has
    sold [num] houses on [property]. Giving the player money is done in the function
    Requires: [player] owns [property], [property] is not mortgaged,
    num_houses [property] is >= num, [property] is a colored property 
    Raises: Failure if preconditions violated*)
val sell_houses : t -> Player.t -> Property.t -> int -> t

(** [mortgage state player property] is the state where [player] has mortgaged
    [property], and recieved money for it.
    Requires: [player] owns property, [property] is unmortgaged,
    [property] is a commercial [property], if [property] is colored then
    [property] has zero houses and all properties of the same color also have 
    zero houses
    Raises: Failure if precondition violated*)
val mortgage : t -> Player.t -> Property.t -> t

(** [unmortgage state player property] is the state where [player]
    has unmortgaged [property], and paid money for it 
    Requires: [player] own property, [property] is mortgaged, 
    [property] is a commercial property, [player] can pay cash to unmortgage
    Raises: Failure if precondition violated*)
val unmortgage : t -> Player.t -> Property.t -> t

(** [buy_property state property player] is the new state in which 
    [player] owns [property], and the appropriate amount of money is taken away 
    from [player] due to the [property]'s price. The new state is in the final phase. 
    Requires: [player] can pay for [property] in cash,
    [property] is a commercial property, and is currently owned by the Bank
    Raises: Failure if preconditions are violated
*)
val buy_property : t -> Property.t -> Player.t -> t

(** [pay state paying_player recieving_player_opt amt] is the new state
    in which [paying_player] has payed [recieving_player_opt] the amount designated 
    by [amt] with the phase set to Final.
    Requires: [paying_player] can pay [amt] in cash
    Raises: Failure if preconditions are violated *)
val pay : t -> Player.t -> Player.t option -> int -> t

(** [move_to_final_phase state] is a new state with its phase set to Final*)
val move_to_final_phase : t -> t

(** [string_of_phase phase] is a string representation of the phase*)
val string_of_phase : phase -> string

(** [get_property name state] is the property with name [name] in [state]
    Requires: there exists a property with name [name] (case_insensitive)
    Raises: Failure if precondition is violated*)
val get_property : t -> string -> Property.t


(**[get_house_mortgage_for_board state] returns a (int * int) list representing the number 
   of houses on each property location (property loc, number of houses) for the given 
   [state]. Order does not matter. (property loc, -1) means the property is mortgaged.
   If the property has no houses and is not mortgaged it is not in the list
   Example: if location 1 (med ave) has 3 houses, and no other properties have houses
   this would return [(1,3)]. If many properties have houses it would return something 
   like: [(1,3);(13,1);(39,4);(23,2);...;(3,2);(6,-1)] where location 6 is mortgaged*)
val get_house_mortgage_for_board : t -> (int*int) list

(** [get_player_by_name state name] is the player with name [name] case insensitive
    Requires: there is a player with name [name] in [state]
    Raises: Failure if precondition is violated*)
val get_player_by_name : t -> string -> Player.t


(** [move_to_trade_phase other_player state]  is a new state in 
    which the phase is set to [Trading]. 
    Requires: [other_player] is not the current player
    [other_player] is not bankrupt
    current phase is either [Rolling] or [Final]
    Raises: Failure if precondition is violated*)
val move_to_trade_phase : Player.t -> t -> t


(** [trade_cash_for_property (p1, cash) (p2, property) state] is the state
    where [p1] has traded [cash] for [p2]'s property [property]
    Requires: [p1] has that amount of cash available
        [p2] owns property [property]
        [p1] and [p2] are not bankrupt
        [p1] and [p2] are not the same player
        if [property] is colored then all properties of the same 
        color have zero houses
        [state] is in the [Trading] phase
    Raises: Failure if precondition is violated*)
val trade_cash_for_property : (Player.t * int) -> (Player.t * Property.t) -> t -> t

(** [trade_prop_for_prop (p1, prop1) (p2, prop2) state] is the state
    where [p1] has traded [prop1] for [p2]'s [prop2]
    Requires: [p1] owns [prop1]
    [p2] owns [prop2]
    [p1] and [p2] are not the same player
    [p1] and [p2] are not bankrupt
    for both [prop1] and [prop2] if colored then all properties of the same 
    color have zero houses
    [state] is in the [Trading] phase
    Raises: Failure if precondition is violated*)
val trade_prop_for_prop : (Player.t * Property.t) -> (Player.t * Property.t) -> t -> t

(** [leave_trading_phase state] is the state back in the phase before trading phase
    was entered 
    Requires: [state] is in [Trading] phase
    Raises: Failure if precondition is violated*)
val leave_trading_phase : t -> t


(** [move_to_auction_phase state] is the state with the phase set to 
    [Auction]
    Requires: [state] is in [Landing Buy phase]
    Raises: Failure if precondition is violated*)
val move_to_auction_phase : t -> t

(** [run_auction state] is the state after an auction has been run 
    (will be in [Final] phase). Note: This is the only funciton in state that
    has I/O permissions
    Requires: [state] is in Auction phase
    Raises: Failure if precondition is violated*)
val run_auction : t -> t

(** [send_current_player_to_jail state] is [state] with the current player on the
    jail property *)
val send_current_player_to_jail : t -> t

(** [get_properties state] is all the properties of the [state] *)
val get_properties : t -> Property.t list


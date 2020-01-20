
(* TYPE DEFINITIONS *)
type scenario = 
  | Buy of Property.t
  | Pay of (Property.t * Player.t option * int)
  | Nil of Property.t

type phase =
  | Rolling
  | Landing of scenario
  | Auction of Property.t
  | Trading of (phase*Player.t)
  | Final

(* turn starts at 0. So when turn is zero it is becasue the player who is at index 0
   of the players field is up. Note: The player has no field that specifies their index
   only their name. Their index is just their index in the list. Same thing for properties*)
type t = {players : Player.t list; properties : Property.t list; turn : int; 
          phase: phase; complete : bool;}


(* HELPER FUNCTIONS *)

(** [make_init_player_lst id names] creates a list of players with names specified by [names] 
    The first name entered is the player who is the first element in the players field
    in the record that represents the state, and is given an [id] of 0. Call the function
    with id set to 0.*)
let rec make_init_player_lst id = function
  | [] -> []
  | h::t -> Player.make id h :: (make_init_player_lst (id+1) t)


(** [player_owns_property player property] is true if [player] owns [property] false
    otherwise*)
let player_owns_property player property =
  if property |> Property.is_commercial then 
    match property |> Property.get_owner with
    | Some id -> id = (player |> Player.get_id) 
    | None -> false 
  else 
    false

(** [get_bid state] is the max bid in the form (Some player, amt)
    Requires: [state] is in auction phase
    Raises: Failure if precondition is violated*)
let get_bid state = 
  match state.phase with
  | Auction prop -> 
    print_endline ("Auction for " ^ (prop |> Property.get_name) ^ ". Retail price: $" 
                   ^ (prop |> Property.get_price |> string_of_int));
    List.fold_left (fun left_sol p -> 
        if p |> Player.get_bankrupt then left_sol
        else
          ((print_string ((p |> Player.get_name) ^ " you have $" ^ 
                          (p |> Player.get_money |> string_of_int) ^ 
                          ", enter your bid (invalid bids count as 0): "));
           (try let amt = read_line () |> int_of_string in
              if amt |> Player.can_pay_cash p  && amt > (snd left_sol) then 
                (Some p, amt) else left_sol
            with | _ -> left_sol))) (None, -1) state.players
  | _ -> failwith "get_bid precondition violated"



(* FUNCTIONS IN INTERFACE *)
let make_init_state names = {players = names |> make_init_player_lst 0; 
                             properties = Properties.make_properties_list (); 
                             turn = 0; 
                             phase = Rolling; complete = false;}                                

let get_player state index = 
  List.nth state.players index

let get_current_player state = 
  List.nth state.players state.turn

let get_turn state = state.turn

(* Note the general strategy of replacing a single player in the player list of the state.
   This will also be useful for updating properties.*)
let move_player state player num = 
  let state' = 
    {state with 
     players = List.map (fun p -> 
         if p = player then Player.move_player p num else p) state.players} in

  let player' = state' |> get_current_player in

  let landing_prop = 
    player' |> Player.get_loc |> List.nth state'.properties in

  match landing_prop with
  | Commercial ct -> 
    (match landing_prop |> Property.get_owner with
     | Some index -> 
       if index = (player' |> Player.get_id) || landing_prop |> 
                                                Property.property_is_mortgaged 
       then 
         {state' with phase = Landing (Nil landing_prop)}
       else 
         {state' 
          with phase = 
                 Landing (Pay (landing_prop, 
                               Some (index |> get_player state'),
                               landing_prop |> 
                               Properties.commercial_rent state'.properties num))}

     | None ->  {state' with phase = Landing (Buy landing_prop)})

  |  NonCommercial nct -> 
    (match nct with
     | Tax (_, _) -> 
       {state' with phase = Landing (Pay (landing_prop, None, Consts.tax_money))}

     | GoJail (_, _) -> {state' with phase = Landing (Pay (landing_prop, None, 
                                                           Consts.jail_money))}

     | _ -> {state' with phase = Landing (Nil landing_prop)})


let rec next_turn state = 
  if state.complete then failwith "next_turn precondition violated"
  else if 
    (state.turn + 1) mod Consts.num_players |> get_player state |> Player.get_bankrupt
  then next_turn {state with turn = (state.turn + 1) mod Consts.num_players}
  else {state with turn = (state.turn + 1) mod Consts.num_players; phase = Rolling}

let get_phase state = state.phase


let get_players_properties state player = 
  Properties.get_player_properties (player |> Player.get_id) state.properties


let give_money state player amount = 
  if amount < 0 then failwith "Don't give negative money" else
    {state with players = List.map (fun ele -> 
         if ele = player then ele |> Player.give_money amount
         else ele) state.players}

let get_location state player = 
  Player.get_loc player |> List.nth state.properties 

let get_all_players state = state.players

let set_property_owner state (property:Property.t) player = 
  match property with
  | Commercial _ -> 
    {state with properties = 
                  List.map 
                    (fun p -> 
                       if p = property then Property.set_owner 
                           (player |> Player.get_id) p 
                       else p) state.properties}
  | NonCommercial _ -> failwith "don't set owner on a non-commercial property"

let is_complete state = state.complete

let get_winner_name state = 
  if not state.complete then failwith "game not done" else 
    List.fold_left (fun left_solution ele -> if left_solution <> "" then left_solution
                     else if ele |> Player.get_bankrupt then left_solution 
                     else ele |> Player.get_name) "" state.players

let take_money player amt state = 
  if player |> Player.get_money < amt then raise (Failure "will result 
  negative money")
  else if amt < 0 then failwith "taking negative money"
  else 
    {state with players = List.map 
                    (fun p -> if p = player then p |> Player.take_money amt 
                      else p) state.players}


let can_pay_total state player amt = 
  (player |> Player.get_money) + 
  (Properties.value_of_assets (Player.get_id player) state.properties) >= amt


let set_player_bankrupt state player = 
  if player |> Player.get_bankrupt || state.complete
  then failwith "violated set_player_bankrupt precondition"
  else { 
    players = 
      List.map 
        (fun p -> if p = player 
          then p |> Player.declare_bankruptcy
          else p) state.players;

    properties = 
      List.map (fun (p: Property.t) -> 
          match p with
          | Commercial ct -> 
            (match p |> Property.get_owner with
             | Some index -> if index = (player |> Player.get_id) 
               then p |> Property.reset_property
               else p
             | None -> p)
          | NonCommercial _ -> p
        ) state.properties;

    turn = (state |> next_turn).turn;

    phase = Rolling;

    complete = 
      List.fold_left (fun left_solution p -> 
          if p |> Player.get_bankrupt then left_solution + 1
          else left_solution) 0 state.players = (Consts.num_players - 2)}


let buy_houses state player (property:Property.t) num = 
  match property with 
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       if 
         info.num_houses + num > 5 ||
         not (Player.can_pay_cash player (info.build_costs * num)) ||
         not (Properties.can_buy_house (player |> Player.get_id) state.properties 
                property)
       then failwith "buy_houses precondition violated"
       else 
         {state with
          players = 
            List.map (fun p -> 
                if p = player then Player.take_money (info.build_costs * num) p 
                else p) state.players;
          properties = 
            List.map (fun p -> 
                if p = property then Property.buy_houses num p
                else p) state.properties}
     | _ -> failwith "buy_houses precondition violated")
  | NonCommercial _ -> failwith "buy_houses precondition violated"

let buy_property state (property:Property.t) player = 
  match property with
  | Commercial ct -> 
    (match property |> Property.get_owner with
     | Some index -> failwith "buy_property precondition violated"
     | None -> 
       if Player.can_pay_cash player (property |> Property.get_price)
       then {state with 
             players = List.map (fun p ->
                 if p = player then p |> Player.take_money 
                                      (property |> Property.get_price)
                 else p) state.players;
             properties = List.map (fun p -> 
                 if p = property then p |> Property.set_owner (player |> Player.get_id)
                 else p) state.properties;
             phase = Final}
       else failwith "buy_property precondition violated"
    )
  | NonCommercial _ -> failwith "buy_property precondition violated"

let pay state paying_player recieving_player_opt amt = 
  if Player.can_pay_cash paying_player amt then 
    match recieving_player_opt with 
    | Some reciever -> 
      {state with players = 
                    List.map (fun p -> 
                        if p = paying_player then p |> Player.take_money amt
                        else if p = reciever then p |> Player.give_money amt
                        else p) state.players;
                  phase = Final}
    | None -> 
      {state with players = 
                    List.map (fun p -> 
                        if p = paying_player then p |> Player.take_money amt
                        else p) state.players;
                  phase = Final}
  else failwith "pay preconditions violated"

let move_to_final_phase state = 
  {state with phase = Final}

let string_of_phase = function
  | Rolling -> ("rolling phase")
  | Landing Buy _ -> ("landing buy phase")
  | Landing Pay _ -> ("landing pay phase")
  | Landing Nil _ -> ("landing nil phase")
  | Auction _ -> ("auction phase")
  | Trading _ -> ("trading phase")
  | Final -> ("final phase")

let get_property state name = 
  let rec helper name = function
    | [] -> failwith "get_property precondition violated"
    | h::t -> if 
      h |> Property.get_name |> String.lowercase_ascii = 
                                (name |> String.lowercase_ascii)
      then h else helper name t in
  helper name state.properties

let sell_houses state player (property:Property.t) num = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       (match info.owner with
        | Some index -> 
          if index = (player |> Player.get_id) && info.num_houses >= num 
             && not info.is_mortgaged
          then  
            {state with 
             players =
               List.map (fun p -> 
                   if p = player 
                   then p |> Player.give_money ((num * info.build_costs)/2)
                   else p) state.players; 
             properties = 
               List.map (fun p -> 
                   if p = property 
                   then p |> Property.sell_houses num
                   else p) state.properties}

          else failwith "sell_houses precondition violated"
        | None -> failwith "sell_houses precondition violated")
     | _ -> failwith "sell_houses precondition violated")
  | NonCommercial _ -> failwith "sell_houses precondition violated"

let mortgage state player (property: Property.t) = 
  match property with
  | Commercial ct -> 
    if Properties.can_mortgage (player |> Player.get_id) state.properties property
    then 
      {state with
       players = 
         List.map (fun p -> 
             if p = player then p |> 
                                Player.give_money 
                                  ((property |> Property.get_price) / 2)
             else p) state.players;
       properties =
         List.map (fun p -> 
             if p = property then p |> Property.set_mortgaged else p) 
           state.properties}
    else failwith "mortgage precondition violated"
  | NonCommercial _ -> failwith "mortgage precondition violated"

let unmortgage state player (property : Property.t) = 
  match property with
  | Commercial ct -> 
    (match property |> Property.get_owner with
     | Some index -> 
       if player |> Player.get_id = index && 
          (property |> Property.property_is_mortgaged)
          && Player.can_pay_cash player ((Property.get_price property) / 2)
       then 
         {state with
          players = 
            List.map (fun p -> 
                if p = player then p |> Player.take_money 
                                     ((Property.get_price property) / 2) else p)
              state.players;
          properties = 
            List.map (fun p -> 
                if p = property then p |> Property.set_unmortgaged else p) 
              state.properties}
       else failwith "unmortgage precondition violated"
     | None -> failwith "unmortgage precondition violated")
  | NonCommercial _ -> failwith "unmortgage precondition violated"

let get_house_mortgage_for_board state = 
  List.fold_left (fun left_sol (ele:Property.t) -> 
      match ele with
      | Commercial ct -> 
        (match ct with
         | Colored i -> 
           if i.is_mortgaged 
           then (Properties.get_property_index ele state.properties, -1) :: left_sol
           else if i.num_houses = 0 then left_sol 
           else (Properties.get_property_index ele state.properties, i.num_houses) 
                :: left_sol
         | Railroad i -> 
           if i.is_mortgaged 
           then (Properties.get_property_index ele state.properties, -1) :: left_sol
           else left_sol
         | Utility i -> if i.is_mortgaged 
           then (Properties.get_property_index ele state.properties, -1) :: left_sol
           else left_sol)
      | NonCommercial _ -> left_sol) [] state.properties

let get_player_by_name state name = 
  try List.find (fun player -> 
      player |> Player.get_name |> String.lowercase_ascii = 
                                   (name |> String.lowercase_ascii)) 
      state.players with
  | Not_found -> failwith "get_player_by_name precondition violated"

let move_to_trade_phase other_player state = 
  match state.phase with
  | Rolling | Final -> 
    if other_player = (state |> get_current_player) || other_player 
                                                       |> Player.get_bankrupt
    then 
      failwith "move_to_trade_phase precondition violated" 
    else {state with phase = Trading (state.phase, other_player)}
  | _ -> failwith "move_to_trade_phase precondition violated"

let trade_cash_for_property (p1, cash) (p2, property) state =
  match state.phase with
  | Trading _ -> 
    if p1 = p2 || not (Player.can_pay_cash p1 cash) 
       || not (player_owns_property p2 property) 
       || not (Properties.no_houses_on_color property state.properties)
       || p1 |> Player.get_bankrupt || p2 |> Player.get_bankrupt
    then failwith "trade_cash_for_property precondition violated"
    else 
      {state with 
       players = List.map (fun p -> 
           if p = p1 then p |> Player.take_money cash
           else if p = p2 then p |> Player.give_money cash
           else p) state.players;

       properties = List.map (fun p ->
           if p = property then p |> Property.set_owner (Player.get_id p1)
           else p ) state.properties}
  | _ -> failwith "trade_cash_for_property precondition violated"

let trade_prop_for_prop (p1, prop1) (p2, prop2) state = 
  match state.phase with
  | Trading _ -> 
    if p1 = p2 || not (player_owns_property p1 prop1) ||
       not (player_owns_property p2 prop2)
       || not (Properties.no_houses_on_color prop1 state.properties)
       || not (Properties.no_houses_on_color prop2 state.properties)
       || p1 |> Player.get_bankrupt || p2 |> Player.get_bankrupt
    then failwith "trade_prop_for_prop precondition violated"
    else 
      {state with 
       properties = List.map (fun p -> 
           if p = prop1 then p |> Property.set_owner (Player.get_id p2)
           else if p = prop2 then p |> Property.set_owner (Player.get_id p1)
           else p) state.properties}
  | _ -> failwith "trade_prop_for_prop precondition violated"

let leave_trading_phase state = 
  {state with 
   phase = 
     match state.phase with
     | Trading (prev_phase, _) -> prev_phase
     | _ -> failwith "leave_trading_phase precondition violated"}

let move_to_auction_phase state = 
  {state with
   phase = 
     match state.phase with
     | Landing (Buy prop) -> Auction prop
     | _ -> failwith "move_to_auction_phase precondition violated"}


let rec run_auction state = 
  match state.phase with 
  | Auction prop -> 
    (match get_bid state with
     | (Some player, amt) -> 
       print_endline ((player |> Player.get_name) ^ " bought " ^ 
                      (prop |> Property.get_name) ^ " for $" ^ 
                      (amt |> string_of_int));
       {state with 
        players = List.map (fun p -> 
            if p = player then p |> Player.take_money amt
            else p) state.players;

        properties = List.map (fun p -> 
            if p = prop then p |> Property.set_owner (player |> Player.get_id)
            else p) state.properties;

        phase = Final}


     | (None, _) -> print_endline "Someone must win auction. Try again"; 
       run_auction state)
  | _ -> failwith "run_auction precondition violated"


let get_properties state = state.properties

let send_current_player_to_jail state =  
  let current_player = state |> get_current_player in
  {state with 
   players = List.map (fun p ->
       if p = current_player then p |> Player.send_to_jail
       else p) state.players}



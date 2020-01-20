open State
open Property

type check = 
  | MoneyCheck
  | PropertyCheck

type command = 
  | Roll of int
  | BuyProperty
  | PassProperty
  | BuyHouses of int * Property.t
  | SellHouses of int * Property.t
  | Mortgage of Property.t
  | Unmortgage of Property.t
  | Pay
  | Check of check
  | TradeWith of Player.t
  | TradeForCash of Property.t * int
  | TradeForProp of Property.t * Property.t
  | TradeCancel
  | EndTurn
  | DeclareBankruptcy



exception Invalid_command
exception Wrong_phase_command


let random_num () = Random.self_init (); (Random.int 11) + 2

(***********************************PARSER************************************)
(** [command_builder state tokens] is the command that [tokens] specify in 
    [state]. Raises: Invalid_command if there is no such command*)
let command_builder state tokens = 

  if tokens = [] then raise Invalid_command

  else if List.hd tokens = "roll" then
    if List.length tokens = 1 then Roll (random_num ())
    else if List.length tokens = 2 then 
      try Roll (int_of_string (List.nth tokens 1))
      with | _ -> raise Invalid_command
    else raise Invalid_command

  else if tokens |> String.concat " " = "buy" then BuyProperty

  else if tokens |> String.concat " " = "pass" then PassProperty


  else if Str.string_match (Str.regexp {|buy [1-5] \(house\|houses\) on [.]*|}) 
      (tokens |> String.concat " ") 0 then 
    (* returns the list of tokens containing the property name *)
    try
      let rec helper = function
        | [] -> failwith "impossible case"
        | h::t -> if h = "on" then t else helper t in

      BuyHouses (int_of_string (List.nth tokens 1), 
                 tokens
                 |> helper
                 |> String.concat " "
                 |> State.get_property state)
    with
    |Failure _ -> raise Invalid_command

  else if Str.string_match (Str.regexp {|sell [1-5] \(house\|houses\) on [.]*|}) 
      (tokens |> String.concat " ") 0 then 
    (* returns the list of tokens containing the property name *)
    try
      let rec helper = function
        | [] -> failwith "impossible case"
        | h::t -> if h = "on" then t else helper t in

      SellHouses (int_of_string (List.nth tokens 1), 
                  tokens
                  |> helper
                  |> String.concat " "
                  |> State.get_property state)
    with
    |Failure _ -> raise Invalid_command

  else if List.hd tokens = "mortgage" && List.length tokens > 1 then 
    try 
      Mortgage ((
          match tokens with 
          | [] -> failwith "impossible case" 
          | h::t -> t) |> String.concat " " |> State.get_property state)
    with
    | Failure _ -> raise Invalid_command

  else if List.hd tokens = "unmortgage" && List.length tokens > 1 then 
    try 
      Unmortgage ((match tokens with
          | [] -> failwith "impossible case"
          | h::t -> t) |> String.concat " " |> State.get_property state)
    with
    | Failure _ -> raise Invalid_command

  else if List.length tokens = 3 && List.nth tokens 0 = "trade"
          && List.nth tokens 1 = "with" then 
    try
      let other_player = State.get_player_by_name state (List.nth tokens 2) in
      TradeWith (other_player)
    with
    | Failure _ -> raise Invalid_command

  else if Str.string_match (
      Str.regexp {|^trade \(\$[0-9]+\|[A-z ]+\) for \(\$[0-9]+\|[A-z ]+\)$|}) 
      (tokens |> String.concat " ") 0 then
    try
      let item_list = Str.split (Str.regexp {|\(trade \| for \)|}) 
          (String.concat " " tokens) in
      let item_1 = List.nth item_list 0 in
      let item_2 = List.nth item_list 1 in
      let is_cash_1 = String.contains item_1 '$' in 
      let is_cash_2 = String.contains item_2 '$' in 
      match is_cash_1,is_cash_2 with 
      | true, true -> raise Invalid_command
      | true, false -> 
        TradeForCash ((State.get_property state item_2),
                      ((String.length item_1)-1
                       |> String.sub item_1 1
                       |> int_of_string))
      | false, true ->
        TradeForCash ((State.get_property state item_1),
                      ((String.length item_2)-1
                       |> String.sub item_2 1
                       |> int_of_string))
      | false, false -> 
        TradeForProp ((State.get_property state item_1),
                      (State.get_property state item_2))
    with
    | Failure _ -> raise Invalid_command
    | Invalid_argument _ -> raise Invalid_command
    | _ -> raise Invalid_command

  else if tokens |> String.concat " " = "cancel"
  then TradeCancel

  else if tokens |> String.concat " " = "pay"
  then Pay

  else if tokens |> String.concat " " = "check money"
  then Check MoneyCheck

  else if tokens |> String.concat " " = "check properties"
  then Check PropertyCheck

  else if tokens |> String.concat " " = "end turn"
  then EndTurn

  else if tokens |> String.concat " " = "declare bankruptcy"
  then DeclareBankruptcy

  else if tokens |> String.concat " " = "quit"
  then exit 0

  else raise Invalid_command



(*******************************COMMAND EXECUTORS*****************************)
(** [sell_houses_command_executor state command] is (state', msg) where state' 
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requires: [command] is [SellHouses] command and [state] is in phase 
    [Rolling], [Landing Buy], [Landing Pay], or [Final]
    Raises: Failure if precondition is violated*)
let sell_houses_command_executor state command = 
  match state |> get_phase with
  | Rolling | Landing Buy _ | Landing Pay _ | Final -> 
    (match command with
     | SellHouses (num, property) -> 
       (try (State.sell_houses state
               (state |> State.get_current_player) property num,
             "You sold " ^ string_of_int num ^ " houses on " ^
             (property |> get_name))
        with Failure _ -> (state, "You can't do that, try something else"))
     | _ -> failwith "sell_houses_command_executor precondition violated")
  | _ -> failwith "sell_houses_command_executor precondition violated"

(** [mortgage_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done. 
    Requires: [command] is [Mortgage] command and [state] is in phase 
    [Rolling], [Landing Buy], [Landing Pay], or [Final]
    Raises: Failure if precondition is violated*)
let mortgage_command_executor state command = 
  match state |> get_phase with
  | Rolling | Landing Buy _ | Landing Pay _ | Final -> 
    (match command with
     | Mortgage property -> 
       (try (mortgage state (state |> State.get_current_player) property,
             "You mortgaged " ^ (property |> get_name))
        with | Failure _ -> (state, "You can't do that, try something else"))
     | _ -> failwith "mortgage_command_executor precondition violated")
  | _ -> failwith "mortgage_command_executor precondition violated"


(** [unmortgage_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done. 
    Requires: [command] is [Unmortgage] command and [state] is in phase 
    [Rolling], or [Final]
    Raises: Failure if precondition is violated*)
let unmortgage_command_executor state command = 
  match state |> get_phase with
  | Rolling | Final -> 
    (match command with
     | Unmortgage property -> 
       (try (unmortgage state (state |> State.get_current_player) property,
             "You unmortgaged " ^ (property |> get_name))
        with | Failure _ -> (state, "You can't do that, try something else"))
     | _ -> failwith "unmortgage_command_executor precondition violated")
  | _ -> failwith "unmortgage_command_executor precondition violated"


(** [rolling_command_executor state command num] is (state', msg)
    where state' is [state] after [command] has been executed, and msg is
    an appropriate msg saying what actions have been done. 
    Requires: command is a [Roll] command and state is in the [Rolling] phase
    Raises: Failure if precondition is violated*)
let rolling_command_executor state command =
  match state |> get_phase with
  | Rolling -> 
    (match command with
     | Roll num -> 

       let new_state = move_player state (state |> get_current_player) num in
       let properties = State.get_properties new_state in 
       let passed_go = ((Properties.get_property_index 
                           (State.get_current_player state
                            |>State.get_location state)
                           properties) + num >= Consts.num_spaces) in
       let state' =
         (if passed_go then
            State.give_money new_state (State.get_current_player new_state) 200 
          else new_state) in
       let go_m = (if passed_go then "You passed go and received $200!\n\n"
                   else "") in

       (match state' |> get_phase with
        | Landing Buy property ->
          (state', go_m^"You rolled a "^string_of_int num^" and landed on "^ 
                   (property |> get_name) ^ ", would you like to buy or pass?")

        | Landing Pay (property, owner, amt) ->
          (match property with
           | Commercial ct -> 
             (match owner with
              | Some player -> 
                (state', "You landed on " ^ (property |> Property.get_name)
                         ^ " and need to pay " ^ (player |> Player.get_name) 
                         ^ " $" ^ (amt |> string_of_int))
              | None -> failwith "impossible case Landing Pay")
           | NonCommercial nct -> 
             (match nct with
              | Tax _ | GoJail _ ->
                (state', "You landed on " ^ (property |> Property.get_name)
                         ^ " and need to pay $" ^ (amt |> string_of_int)) 
              | _ -> failwith "impossible case"))

        | Landing Nil property -> 
          (match property with
           | Commercial ct -> 
             (state' |> move_to_final_phase, 
              go_m^"You rolled a " ^ string_of_int num ^ " and landed on " ^ 
              (property |> get_name) ^ ", there is nothing to do")

           | NonCommercial nct -> 
             (match nct with
              | Go _ | Jail _ | FreeParking _ | CCC _ -> 
                (* TODO: Implement*)
                (state' |> move_to_final_phase, 
                 go_m^"You rolled a " ^ string_of_int num ^ " and landed on " ^ 
                 (property |> get_name) ^ ", there is nothing to do")
              | Tax _ | GoJail _ -> failwith "impossible case roll_executor"))


        | _ -> failwith "impossible case roll_executor")
     | _ -> failwith "rolling_command_exectuer precondition violated")
  | _ -> failwith "rolling_command_exectuter precondition violated"

(** [check_command_executor state command] is (state', msg) where state' is
    [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done. 
    Requires: [state] phase is Rolling, Landing Pay, Landing Buy, or Final 
    [command] is a Check command.
    Raises: Failure if precondition violated*)
let check_command_executor state command = 
  match state |> get_phase with 
  | Rolling | Trading _ | Landing Pay _ | Landing Buy _ | Final -> 
    (match command with
     | Check check -> 
       (match check with
        | MoneyCheck -> 
          (state,"You have $" ^(state |> get_current_player
                                |> Player.get_money |> string_of_int))
        | PropertyCheck -> (state, "You own:\n" ^ 
                                   (state |> get_current_player |> 
                                    get_players_properties state |> 
                                    List.map get_name |> String.concat "\n")))
     | _ -> failwith "check_command_executor precondition violated")
  | _ -> failwith "check_command_executor precondition violated"


(** [buy_houses_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate msg
    saying what actions have been done.
    Requries: [state] is in phase [Rolling] or [Final] and 
      [command] is the [BuyHouses] command
    Raises: Failure if precondition is violated *)
let buy_houses_command_executor state command = 
  match state |> get_phase with
  | Rolling | Final -> 
    (match command with
     | BuyHouses (num, property) -> 
       (try
          (State.buy_houses state (state |> get_current_player) property num,
           "You bought "^string_of_int num^" houses on "^(property|>get_name))
        with
        | Failure _ -> (state, "You can't do that, try something else"))
     | _ -> failwith "buy_houses_command_executor precondition violated")
  | _ -> failwith "buy_houses_command_executor precondition violated"


(** [buy_property_command_executor state command]  is (state', msg) where
    state' is [state] after [command] has been executed, and msg is an
    appropriate msg saying what actions have been done.
    Requries: [state] is in phase Landing Buy and
      [command] is a BuyProperty command
    Raises: Failure if precondition is violated*)
let buy_property_command_executor state command = 
  match state |> get_phase with
  | Landing Buy property -> 
    (match command with
     | BuyProperty -> 
       (try (buy_property state property (state |> get_current_player),
             "You bought " ^ (property |> get_name) ^ "!")
        with Failure _ ->
          (state, "You don't have enough money, try something else"))
     | _ -> failwith "buy_property_command_executor precondition violated")
  | _ -> failwith "buy_property_command_executor precondition violated"

(** [pass_property_command_executor state command]  is (state', msg) where
    state' is [state] after [command] has been executed, and msg is an 
    appropriate msg saying what actions have been done.
    Requries: [state] is in phase Landing Buy and 
      [command] is a PassProperty command.
    Raises: Failure if precondition is violated. *)
let pass_property_command_executor state command = 
  match state |> get_phase with
  | Landing Buy property -> 
    (match command with
     | PassProperty ->  
       let new_state = 
         State.run_auction (State.move_to_auction_phase state) in
       (new_state, "The auction is over!")
     | _ -> failwith "pass_property_command_executor precondition violated")
  | _ -> failwith "pass_property_command_executor precondition violated"

(** [pay_command_executor state command] is (state', msg) where state' is
    [state] after [command] has been executed, and msg is an appropriate msg
    saying what actions have been done.
    Requries: [state] is in phase Landing Pay and [command] is a Pay command
    Raises: Failure if precondition is violated*)
let pay_command_executor state command = 
  match state |> get_phase with
  | Landing Pay (property, reciever, amt) -> 
    (match command with
     | Pay -> 
       (try 
          (match reciever with
           | Some player -> 
             (pay state (state |> get_current_player) reciever amt,
              "You paid $" ^ (string_of_int amt) ^" to "
              ^(Player.get_name player)^".")
           | None -> 
             (match property with
              | Commercial _ -> failwith "impossible case pay_command_executor"
              | NonCommercial nct ->
                (match nct with
                 | Tax _ -> 
                   (pay state (state |> get_current_player) reciever amt,
                    "You paid $" ^ (string_of_int amt)^" in taxes.")
                 | GoJail _ -> 
                   (pay state (state |> get_current_player) reciever amt |> 
                    State.send_current_player_to_jail, 
                    "You paid $"
                    ^ (string_of_int amt) ^ " and were sent to jail")
                 | _ -> failwith "impossible case pay_command ")))
        with Failure _ ->
          (state, "You don't have enough money, try something else"))
     | _ -> failwith "pay_command_executor precondition violated")
  | _ -> failwith "pay_command_executor precondition violated"


(** [declare_bankruptcy_command_executor state command] is (state', msg) where
    state' is [state] after [command] has been executed, and msg is an
    appropriate msg saying what actions have been done.
    Requries: [state] is in phase Landing Pay and [command] is a
      DeclareBankruptcy command
    Raises: Failure if precondition is violated*)
let declare_bankruptcy_command_executor state command = 
  match state |> get_phase with
  | Landing Pay _ -> 
    (match command with
     | DeclareBankruptcy -> 
       (set_player_bankrupt state (state |> get_current_player), 
        (state|>get_current_player|>Player.get_name)^" declared bankruptcy!")
     |_->failwith "declare_bankruptcy_command_executor precondition violated")
  | _ -> failwith "declare_bankruptcy_command_executor precondition violated"

(** [trade_with_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase [Rolling] or [Final] 
      and [command] is a TradeWith command
    Raises: Failure if precondition is violated. *)
let trade_with_command_executor state command =
  try  
    match state |> get_phase with
    | Rolling | Final -> 
      (match command with
       | TradeWith other_player -> 
         ((State.move_to_trade_phase other_player state),
          "You are now in the trading phase!")
       | _ -> failwith "trade_with_command_executor precondition violated")
    | _ -> failwith "trade_with_command_executor precondition violated"
  with Failure _ -> raise Invalid_command

(** [trade_for_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase [Trading] and [command] is a TradeForCash,
      TradeForProp, TradeCancel command.
    Raises: Failure if precondition is violated. *)
let trade_for_command_executor state command = 
  let p_offering = State.get_current_player state in
  match state |> get_phase with 
  | Trading (phase, p_offered) ->(
      try 
        match command with
        | TradeForCash (prop, amt) -> begin
            try
              ((State.trade_cash_for_property (p_offering,amt) (p_offered,prop)
                  state) |> State.leave_trading_phase, "Trade successful!")
            with Failure _ -> 
              ((State.trade_cash_for_property (p_offered,amt) (p_offering,prop)
                  state) |> State.leave_trading_phase, "Trade successful!")
          end
        | TradeForProp (prop_1,prop_2) -> begin
            try
              ((State.trade_prop_for_prop (p_offering,prop_1)
                  (p_offered,prop_2) state)
               |> State.leave_trading_phase, "Trade successful!")
            with Failure _ -> 
              ((State.trade_prop_for_prop (p_offering,prop_2)
                  (p_offered,prop_1) state)
               |> State.leave_trading_phase,"Trade successful!")
          end
        | TradeCancel -> (State.leave_trading_phase state, "Trade canceled!")
        | _ -> failwith "trade_for_command_executor precondition violated"
      with Failure _ -> (state, "That trade is not valid/allowed!"))
  | _ -> failwith "trade_for_command_executor precondition violated"

(** [end_turn_command_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase Final and [command] is a EndTurn 
    Raises: Failure if precondition is violated*)
let end_turn_command_executor state command = 
  match state |> get_phase with
  | Final -> 
    (match command with
     | EndTurn -> 
       (state |> next_turn, "")
     | _ -> failwith "end_turn_command_executor precondition violated")
  | _ -> failwith "end_turn_command_executor precondition violated"



(******************************PHASE EXECUTORS********************************)
(** [roll_phase_executor state command] is (state', msg) where state' is
    [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase [Rolling]
    Raises: Failure if precondition is violated*)
let roll_phase_executor state command = 
  match state |> get_phase with
  | Rolling -> 
    (match command with
     | Roll _ -> rolling_command_executor state command 
     | BuyHouses _ -> buy_houses_command_executor state command
     | SellHouses _ -> sell_houses_command_executor state command
     | Mortgage _ -> mortgage_command_executor state command
     | Unmortgage _ -> unmortgage_command_executor state command
     | TradeWith _ -> trade_with_command_executor state command
     | Check _ -> check_command_executor state command 
     | _ -> raise Wrong_phase_command)
  | _ -> failwith "roll_phase_executor precondition violated"


(** [landing_buy_phase_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase Landing Buy
    Raises: Failure if precondition is violated. *)
let landing_buy_phase_executor state command = 
  match state |> get_phase with
  | Landing Buy _ -> 
    (match command with
     | BuyProperty -> buy_property_command_executor state command
     | PassProperty -> pass_property_command_executor state command
     | Mortgage _ -> mortgage_command_executor state command
     | SellHouses _ -> sell_houses_command_executor state command
     | Check _ -> check_command_executor state command
     | _ -> raise Wrong_phase_command)
  | _ -> failwith "landing_buy_phase_executor precondition violated"


(** [landing_pay_phase_executor state command] is (state', msg) where state'
    is [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase Landing Pay
    Raises: Failure if precondition is violated*)
let landing_pay_phase_executor state command = 
  match state |> get_phase with
  | Landing Pay _ -> 
    (match command with
     | Pay -> pay_command_executor state command
     | DeclareBankruptcy -> declare_bankruptcy_command_executor state command
     | Mortgage _ -> mortgage_command_executor state command
     | SellHouses _ -> sell_houses_command_executor state command
     | Check _ -> check_command_executor state command
     | _ -> raise Wrong_phase_command)
  | _ -> failwith "landing_pay_phase_executor precondition violated"


(** [final_phase_executor state command] is (state', msg) where state' is
    [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase Final
    Raises: Failure if precondition is violated*)
let final_phase_executor state command = 
  match state |> get_phase with
  | Final -> 
    (match command with
     | BuyHouses _ -> buy_houses_command_executor state command
     | SellHouses _ -> sell_houses_command_executor state command
     | Mortgage _ -> mortgage_command_executor state command
     | Unmortgage _ -> unmortgage_command_executor state command
     | TradeWith _ -> trade_with_command_executor state command
     | Check _ -> check_command_executor state command
     | EndTurn -> end_turn_command_executor state command
     | _ -> raise Wrong_phase_command)
  | _ -> failwith "final_phase_executor precondition violated"

(** [trading_phase_executor state command] is (state', msg) where state' is
    [state] after [command] has been executed, and msg is an appropriate
    msg saying what actions have been done.
    Requries: [state] is in phase Trading
    Raises: Failure if precondition is violated *)
let trading_phase_executor state command = 
  match state |> get_phase with
  | Trading _ -> 
    (match command with
     | TradeForCash _ -> trade_for_command_executor state command
     | TradeForProp _ -> trade_for_command_executor state command
     | TradeCancel  -> trade_for_command_executor state command
     | Check _ -> check_command_executor state command
     | _ -> raise Wrong_phase_command)
  | _ -> failwith "trading_phase_executor precondition violated"


(********************************DELEGATORS***********************************)
(** [command_executor state command] is (state', msg) where state' is [state]
    after [command] has been executed, and msg is an appropriate msg
    saying what actions have been done.
    Requires: [state] is not in phase Landing Nil
    Raises: Failure if precondition is violated*)
let command_executor state command =
  match state |> State.get_phase with
  | Rolling -> roll_phase_executor state command
  | Landing Buy _ -> landing_buy_phase_executor state command 
  | Landing Pay _ -> landing_pay_phase_executor state command 
  | Landing Nil _ -> failwith "command_executor precondition violated, nil."
  | Auction _ -> failwith "command_executor precondition violated, auction."
  | Trading _ -> trading_phase_executor state command
  | Final ->  final_phase_executor state command


let command_parser state str = 
  try 
    let tokens = str |> Str.split (Str.regexp "[ \t]+") |> 
                 List.map (fun e -> e |> String.lowercase_ascii) in

    try tokens |> command_builder state |> command_executor state with
    | Invalid_command -> (state, "Please enter a valid command")
    | Wrong_phase_command -> (state,"You can't do that in this phase.")
  with _ -> (state, "Please enter a valid command")

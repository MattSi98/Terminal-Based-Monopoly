open OUnit2
open Property
open Player
open Consts
open State
open Command

(**
    To ensure the porgram operates as we intended, 
    we added tests for each function within the modules 
    (whenever it was possible).
    This allowed us to test state to state transition, functions that altered 
    the state, functions that added/took away houses or money,etc. 
    It also ensures that when new functionality is added, 
    the suite can be run to give a sanity check 
    that nothing broke in the additions. 

    Much of testing had to be play tested. Many boardprinter bugs could only be
    found by playing the board and forcing certain states to be printed.  

    This testing is much like assignmnet 2 in that the game was initially play 
    tested - to ensure the game plays well, and all obvious bug in natural play
    were caght.  While the test suite provides a good
    way of ensuring calculations and state changes are handled properly. 

    We created tests in two ways. 1) test each function individually using a 
    black box style, only using the mli.  This ensured that each individual 
    function operated as intended for base and edge cases. 2) we strung 
    many function calls and state transitions together, and then tested to see
    if the outcome was expected.  This was more of a glass box style of testing
    requiring knowledge of the functions and states in order to predict the
    outcome and test aginst it.  

    Full code coverage was hard to achieve, due to
    the complexity of the game itself.  The amount of states and possible 
    combinations of moves grows exponentially as the game goes on.  Therefore,
    we prioritized ensuring individual function correctness, as best we could,
    rather than trying to test each and every state. 

    Understanding that testing would be difficult, we designed the program to 
    follow a strict module hierarchy.  This helps to ensure that modules can not
    access information or functions that would cause bugs in the program. 
*)

let player1 = Player.make 1 "Matt" 
let bnkrptP1 = Player.declare_bankruptcy player1 
let moved5P1 = Player.move_player player1 5 
let p1_2500dollars = Player.give_money 2000 player1 
let p1_minus2000 = Player.take_money 2000 p1_2500dollars 


let park_place_owned = Property.set_owner 1 park_place 
let pp_mortgaged = Property.set_mortgaged park_place_owned 
let pp_unmortgaged = Property.set_unmortgaged pp_mortgaged
let pp_reset = Property.reset_property park_place_owned
let pp_3houses = Property.buy_houses 3 park_place_owned  
let pp_sell_houses = Property.sell_houses 3 pp_3houses


let init_state = State.make_init_state ["matt";"bob";"sean"] 
let move_player_state = State.move_player init_state 
    (State.get_player init_state 0) 1
let trade_with = fst (Command.command_parser move_player_state "trade with bob")
let next_turn_state = State.next_turn init_state 
let money_given_state = State.give_money init_state 
    (State.get_player init_state 0) 2000
let money_taken_state = State.take_money (State.get_player money_given_state 0) 
    2000 money_given_state
let final_phase_state = State.move_to_final_phase init_state
let trade_phase_state = State.move_to_trade_phase 
    (State.get_player init_state 2) init_state
let auction_state = State.move_to_auction_phase move_player_state
let leave_trading_state = State.leave_trading_phase trade_phase_state

let roll_3 = State.move_player init_state 
    (State.get_player init_state 0) 3
let buy_baltic = State.buy_property roll_3 Property.baltic_ave
    (State.get_player roll_3 0)
let mortgage_baltic = State.mortgage buy_baltic
    (State.get_player buy_baltic 0)
    (State.get_location buy_baltic (State.get_current_player buy_baltic))
let unmortgage_baltic = State.unmortgage mortgage_baltic
    (State.get_player mortgage_baltic 0)
    (State.get_location mortgage_baltic
       (State.get_current_player mortgage_baltic))
let end_turn = State.next_turn buy_baltic
let p2_move3 = State.move_player end_turn (State.get_player end_turn 1) 3
let pay_rent = 
  State.pay (p2_move3) (State.get_player p2_move3 1)
    (Some (State.get_player p2_move3 0)) 4
let roll_4 = State.move_player init_state 
    (State.get_player init_state 0) 4
let dec_bankruptcy = State.set_player_bankrupt roll_4
    (State.get_player roll_4 0)
let trade_with_bankrupt = State.move_player (State.next_turn dec_bankruptcy)
    (State.get_player dec_bankruptcy 1) 10
let pay_tax = State.pay (roll_4) (State.get_player roll_4 0) (None) 150
let roll_30 = State.move_player init_state 
    (State.get_player init_state 0) 30
let pay_jail = (State.pay (roll_30) (State.get_player roll_30 0) (None) 50)
               |> State.send_current_player_to_jail

let suite = "test suite" >::: [

    (**Tests for Player module *)
    "test get name" >:: (fun _ ->
        assert_bool "strings not equal" (Player.get_name player1 = "Matt"));

    "test get id" >:: (fun _ -> 
        assert_bool "id not equal" (Player.get_id player1 = 1));

    "test get money" >:: (fun _ -> 
        assert_bool "money is not equal" (Player.get_money player1 =
                                          Consts.starting_money));

    "test get loc" >:: (fun _  -> assert_bool "loc not equal" 
                           (Player.get_loc player1 = 0));

    "test get bankrupt" >:: (fun _ -> assert_bool "bool not equal" 
                                (Player.get_bankrupt player1 = false));

    "test declare bankrupt" >:: (fun _ -> assert_bool "bool not equal" 
                                    (Player.get_bankrupt bnkrptP1 = true));

    "test move player" >:: (fun _ -> assert_bool "int not equal"
                               (Player.get_loc moved5P1 = 5));

    "test give money" >:: (fun _ -> assert_bool "int not equal" 
                              (Player.get_money p1_2500dollars =
                               (Consts.starting_money + 2000)));

    "test take money" >:: (fun _ -> assert_bool "int not equal" 
                              (Player.get_money p1_minus2000 = 
                               Consts.starting_money));

    (**Tests for Property module *)
    "test set/get owner" >:: (fun _ -> assert_bool "int not equal"
                                 (Property.get_owner park_place_owned =
                                  Some 1));

    "test get owner of non comm" >:: 
    (fun _ -> assert_raises 
        (Failure "Don't get owner of a non-commercial property") 
        (fun () -> Property.get_owner go));

    "test set owner of non comm" >:: 
    (fun _ -> assert_raises 
        (Failure "Don't set owner on a non-commercial property") 
        (fun () -> Property.set_owner 0 go));

    "test get color" >:: (fun _ -> assert_bool "color not equal" 
                             (Property.get_color park_place =
                              Property.DarkBlue));

    "test is comm" >:: (fun _ -> assert_bool "bool not equal"
                           (Property.is_commercial park_place = true));

    "test is noncomm" >:: (fun _ -> assert_bool "bool not equal" 
                              (Property.is_commercial jail = false));

    "test is comm on non comm" >:: 
    (fun _ -> assert_bool "bool not equal"
        (Property.is_commercial go = false));

    "test is noncomm on comm" >:: 
    (fun _ -> assert_bool "bool not equal" 
        (Property.is_commercial reading_railroad = true));  

    "test is mortg" >:: (fun _ -> assert_bool "bool not equal" 
                            (Property.property_is_mortgaged park_place_owned = 
                             false));

    "test set mortaged" >:: (fun _ -> assert_bool "bool not equal" 
                                (Property.property_is_mortgaged pp_mortgaged
                                 = true));

    "test set mortg on no comm" >:: 
    (fun _ -> assert_raises 
        (Failure "set_mortgaged precondition violated")
        (fun () -> Property.set_mortgaged go));

    "test set unmortaged"
    >:: (fun _ -> assert_bool "bool not equal" 
            (Property.property_is_mortgaged pp_unmortgaged
             = false));

    "test set unmortg on no comm" >:: 
    (fun _ -> assert_raises 
        (Failure "set_unmortgaged precondition violated")
        (fun () -> Property.set_unmortgaged go));

    "test reset prop" >:: (fun _ -> assert_bool "some int not equal" 
                              (Property.get_owner pp_reset = None));

    "test buy houses" >:: (fun _ -> assert_bool "int not equal"
                              (Property.get_houses pp_3houses = 3));

    "test by houses on non comm" >:: 
    (fun _ -> assert_raises 
        (Failure "buy_houses precondition violated") 
        (fun () -> Property.buy_houses 4 go));

    "test get price" >:: (fun _ -> assert_bool "int not equal" 
                             (Property.get_price park_place = 350));

    "test get price on non comm prop" >:: 
    (fun _ -> assert_raises 
        (Failure "get_price precondition violated") 
        (fun () -> Property.get_price go));

    "test get name" >::
    (fun _ -> assert_bool "string not equal" 
        (Property.get_name park_place_owned ="Park Place"));

    "test sell houses" >:: (fun _ -> assert_bool "int not equal"
                               (Property.get_houses pp_sell_houses = 0));

    "test sell houses on non comm" >:: 
    (fun _ -> assert_raises 
        (Failure "sell_houses precondition violated") 
        (fun () -> Property.sell_houses 4 go));

    (**Tests for state module *)

    "test get player" >:: (fun _ -> assert_bool "int not equal" 
                              (Player.get_name(State.get_player init_state 0)
                               = "matt"));

    "test get current player" >:: (fun _ -> assert_bool "not equal"
                                      (State.get_current_player init_state = 
                                       State.get_player init_state 0 ));

    "test get turn" >:: (fun _ -> assert_bool "int not equal"
                            (State.get_turn init_state = 0));

    "test move player" >:: (fun _ -> assert_bool "not equal" 
                               (Player.get_loc (State.get_player
                                                  move_player_state 0) 
                                = 1));

    "test get phase" >:: (fun _ -> assert_bool "not equal" 
                             (State.get_phase move_player_state =
                              State.Landing (Buy mediterranean_ave)));

    "test next turn" >:: ( fun _ -> assert_bool "not equal" 
                             (State.get_turn next_turn_state = 1));

    "test get prop" >:: (fun _ -> assert_bool "not equal" 
                            (State.get_players_properties init_state 
                               (State.get_player init_state 0) = []));

    "test give money" >:: (fun _ -> assert_bool "not equal"
                              ((Player.get_money (State.get_player
                                                    money_given_state 0 ))
                               = (Consts.starting_money + 2000)));

    "test get loc" >:: (fun _ -> assert_bool "not equal" 
                           ((State.get_location move_player_state 
                               (State.get_player move_player_state 0)) =
                            mediterranean_ave));

    "test get all players" >:: (fun _ -> assert_bool "not equal" 
                                   (State.get_all_players init_state = 
                                    [State.get_player init_state 0 ;
                                     State.get_player init_state 1;
                                     State.get_player init_state 2]));

    "test is complete" >:: (fun _ -> assert_bool "not equal" 
                               (State.is_complete init_state = false));

    "test take money" >:: ( fun _ -> assert_bool "not equal" 
                              ((Player.get_money(State.get_player 
                                                   money_taken_state 0)) =
                               (Consts.starting_money)));

    "test move to final" >:: (fun _ -> assert_bool "not equal" 
                                 (State.get_phase final_phase_state = 
                                  State.Final));

    "test string of phase" >::
    (fun _ -> assert_bool "not equal" 
        (State.string_of_phase(State.get_phase 
                                 final_phase_state) = 
         "final phase"));

    "test get prop" >:: (fun _ -> assert_bool "not equal" 
                            (State.get_property init_state "boardwalk" = 
                             boardwalk));

    "test get houses for board" >:: (fun _ -> assert_bool "not equal" 
                                        (State.get_house_mortgage_for_board 
                                           init_state = 
                                         []));

    "test get player by name" >:: (fun _ -> assert_bool "not equal" 
                                      (State.get_player_by_name init_state 
                                         "matt"
                                       = State.get_player init_state 0));

    "test move to trade phase" >:: ( fun _ -> assert_bool "not equal"
                                       (State.get_phase trade_phase_state = 
                                        State.Trading 
                                          (State.get_phase init_state,
                                           State.get_player init_state 2)));

    "test move to auction" >:: ( fun _ -> assert_bool "not equal" 
                                   (State.get_phase auction_state =
                                    Auction mediterranean_ave));

    "test leave trading" >:: (fun _ -> assert_bool "not equal" 
                                 (State.get_phase leave_trading_state = 
                                  Rolling));

    (**Tests for command module *)

    "Command move 1" >:: (fun _ -> assert_bool "not equal" 
                             (fst (command_parser init_state "roll 1")
                              = move_player_state));

    "Command roll 3" >:: (fun _ -> assert_bool "not equal" 
                             (fst (command_parser (init_state) "roll 3")
                              = roll_3));

    "Command buy Baltic Av" >:: (fun _ -> assert_bool "not equal" 
                                    (fst (command_parser (roll_3) "buy")
                                     = buy_baltic));

    "end turn" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (buy_baltic) "end turn")
         = end_turn));

    "command pay rent" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (p2_move3) "pay")
         = pay_rent));

    "command pay tax" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (roll_4) "pay")
         = pay_tax));

    "command pay jail" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (roll_30) "pay")
         = pay_jail));

    "command bankruptcy" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (roll_4) "declare bankruptcy")
         = dec_bankruptcy));

    "Pass Go, +200" >::
    (fun _ -> assert_bool "not equal" 
        ((State.get_current_player (fst (command_parser init_state "roll 50"))
          |> Player.get_money) = 1700));

    "trade with bankrupt player" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (trade_with_bankrupt) "trade with Matt")
         = trade_with_bankrupt));

    "command invalid trade" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (trade_with) "trade Mediterranean Avenue for $20")
         = trade_with));

    "buy house invalid" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (buy_baltic) "buy 1 house on Baltic Avenue")
         = buy_baltic));

    "mortgage prop" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (buy_baltic) "mortgage Baltic Avenue")
         = mortgage_baltic));

    "unmortgage prop" >::
    (fun _ -> assert_bool "not equal" 
        (fst (command_parser (mortgage_baltic) "unmortgage Baltic Avenue")
         = unmortgage_baltic));

  ]


let _ = run_test_tt_main suite
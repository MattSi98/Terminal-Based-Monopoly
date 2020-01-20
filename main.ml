
let rec repl state = 
  (*TODO: prompt the player whos turn it is for a command*)
  print_endline ("It is " ^ (state |> State.get_current_player 
                             |> Player.get_name) 
                 ^ "'s turn: (" ^ (State.string_of_phase 
                                     (State.get_phase state)) ^
                 ")");
  print_endline ("What do you want to do " ^ (state |> State.get_current_player 
                                              |> Player.get_name) ^ "?");
  let new_state_and_msg = read_line() |> Command.command_parser state in
  fst new_state_and_msg |> BoardPrinter.print_final_board;
  print_endline (snd new_state_and_msg);
  if not (new_state_and_msg |> fst |> State.is_complete) then 
    repl (fst new_state_and_msg)
  else 
    (print_endline ("The winner is " ^ (new_state_and_msg |> fst 
                                        |> State.get_winner_name) ^ "!");
     print_endline "Thank you for playing!")

(* plays the game *)
let play names  =  State.make_init_state names |> repl

(*prompts the players for their names and returns a list 
  whcih contains the names they entered
   in the order they entered them *)
let rec get_names () = 
  print_endline "Enter names of up to 4 characters, and make them unique!";
  let rec h n = 
    if n = 0 then (print_endline ""; []) else 
      (print_endline ("Please enter player " 
                      ^ string_of_int(Consts.num_players - n) 
                      ^ "'s name:");
       let name = read_line () in
       if name = "" || String.length name > 4 then 
         (print_endline "Invalid name try again"; h n) 
       else
         name :: h (n-1)) in

  let no_dups lst =
    List.fold_left (fun left_sol n ->
        if fst left_sol = false then left_sol
        else if snd left_sol |> List.mem n then (false, snd left_sol)
        else (fst left_sol, n :: snd left_sol)) (true, []) lst |> fst in

  let names = Consts.num_players |> h in
  if names |> no_dups then names else 
    (print_endline "Names were not unique. Try again"; get_names ())

let _ =  print_endline "Welcome to command line interface version of Monopoly!";
  get_names () |> play 
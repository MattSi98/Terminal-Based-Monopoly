(* location specifies an index in the list of properties that the state holds*)
(* id specifies the index in the list of players that the state hold *)
type t = {
  id : int;
  name : string;
  location : int;
  money : int;
  bankrupt : bool}


let make id name = {
  id = id;
  name = name;
  location = 0;
  money = Consts.starting_money;
  bankrupt = false
}

let get_name player = player.name

let get_name_opt player = match player with 
  | Some p -> p.name
  | None -> "the bank"

let get_money player = player.money

let get_id player = player.id

let get_loc player = player.location

let get_bankrupt player = player.bankrupt

let declare_bankruptcy player = {player with bankrupt = true; location = -1}

let move_player player n = 
  {player with location = (player.location + n) mod Consts.num_spaces}

let give_money amt player = {player with money = player.money + amt}

let take_money amt player = 
  if player.money < amt then raise (Failure "negative money") else 
    {player with money = player.money - amt}

let can_pay_cash player amt = 
  player |> get_money >= amt

let send_to_jail player = 
  {player with location = Consts.jail_pos}
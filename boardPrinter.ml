open Player
open State
open Property


module IntTupl = struct
  type t = int * int 
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end;;

module Dic = Map.Make(IntTupl) 

let empty_dic = Dic.empty


(**[player_pos_to_tupl int] is the tupl representation of the players current
   location [int] on the board 
   There are 40 locations, and each correspond to a property*)
let player_pos_to_tupl int =
  match int with
  | -1 -> (30,10)
  | 0 -> (33,11) | 1 -> (33,10) | 2 -> (33,9)  | 3 -> (33,8)  | 4 -> (33,7) 
  | 5 -> (33,6)  | 6 -> (33,5)  | 7 -> (33,4)  | 8 -> (33,3)  | 9 -> (33,2) 
  | 10 -> (33,1) | 11 -> (30,1) | 12 -> (27,1) | 13 -> (24,1) | 14 -> (21,1)
  | 15 -> (18,1) | 16 -> (15,1) | 17 -> (12,1) | 18 -> (9,1)  | 19 -> (6,1)
  | 20 -> (3,1)  | 21 -> (3,2)  | 22 -> (3,3)  | 23 -> (3,4)  | 24 -> (3,5)  
  | 25 -> (3,6)  | 26 -> (3,7)  | 27 -> (3,8)  | 28 -> (3,9)  | 29 -> (3,10) 
  | 30 -> (3,11) | 31 -> (6,11) | 32 -> (9,11) | 33 -> (12,11)| 34 -> (15,11) 
  | 35 -> (18,11)| 36 -> (21,11)| 37 -> (24,11)| 38 -> (27,11)| 39 -> (30,11)
  | n -> if (n > 0) then 
      (if(n > 39) then (raise (Failure "pos not in range 0..39")) 
       else (99,99)) else (raise (Failure "pos is not in range 0..39"))

(**[padd_name name num] is [name] with spaces concatted onto it until the len 
   is 7*)
let padd_name name num =
  let padding = String.make num ' ' in 
  name ^ padding 

let name_padded name =
  if (String.length name > 7)
  then (String.sub name 0 7)
  else (padd_name name (9 - String.length name))

(**[name_to_cell name] is the string "|<padded name>|"
   if len [name] is > 7 it is truncated
   if let [name] is < 7 it is padded with spaces  *)
let name_to_cell name = 
  if (String.length name > 7) 
  then (let sub = String.sub name 0 7 in
        let lst = ["|";sub;"|"] in
        String.concat "" lst ) 
  else (
    if (String.length name < 7) 
    then (let padded = padd_name name (7 - String.length name) in
          let lst = ["|";padded;"|"] in
          String.concat "" lst ) 
    else (let lst = ["|";name;"|"] in 
          String.concat "" lst )
  )

(**[player_loc_list lst loc] is the tuple list of location tuples that map 
   players positions to player names 
   ((int*int)*string) list 
   ex [((3,1),"|Matthew|");((3,11),"|Sean   |")]*)
let rec player_loc_list lst acc =
  match lst with 
  | [] -> acc
  | h::t -> player_loc_list t ((player_pos_to_tupl (get_loc h), 
                                if (player_pos_to_tupl (get_loc h) = (30,10)) 
                                then "         " 
                                else (name_to_cell (Player.get_name h))
                               ) :: acc)

(**[house_num_padding num] returns the string representation of [num] the number
   of houses on a property *)
let house_num_padding num =
  match num with 
  | -1 ->"| Mortg |"
  | 0 -> "|       |"
  | 1 -> "|  ⌂    |"
  | 2 -> "|  ⌂⌂   |"
  | 3 -> "|  ⌂⌂⌂  |"
  | 4 -> "|  ⌂⌂⌂⌂ |"
  | 5 -> "| Hotel |"
  | n -> "|       |"

(**[house_num_list lst acc] is the (int*int)*string list representing the 
    keys and values to be added when updating the baord.  These vlaues are 
    calculated from [lst] which is a list representing house numbers on 
    property locations.*)
let rec house_num_list lst acc = 
  match lst with 
  | [] -> acc
  | (pos,houses)::t -> 
    match pos with 
    | 1 ->house_num_list t (((32,10), house_num_padding houses)::acc)  
    | 3 ->house_num_list t (((32,8), house_num_padding houses)::acc)   
    | 6 ->house_num_list t (((32,5), house_num_padding houses)::acc)   
    | 8 ->house_num_list t (((32,3), house_num_padding houses)::acc)    
    | 9 ->house_num_list t (((32,2), house_num_padding houses)::acc)   
    | 11 ->house_num_list t (((29,1), house_num_padding houses)::acc)   
    | 13 ->house_num_list t (((23,1), house_num_padding houses)::acc)  
    | 14 ->house_num_list t (((20,1), house_num_padding houses)::acc)
    | 16 ->house_num_list t (((14,1), house_num_padding houses)::acc)  
    | 18 ->house_num_list t (((8,1), house_num_padding houses)::acc) 
    | 19 ->house_num_list t (((5,1), house_num_padding houses)::acc)
    | 21 ->house_num_list t (((2,2), house_num_padding houses)::acc)   
    | 23 ->house_num_list t (((2,4), house_num_padding houses)::acc)  
    | 24 ->house_num_list t (((2,5), house_num_padding houses)::acc)  
    | 26 ->house_num_list t (((2,7), house_num_padding houses)::acc)   
    | 27 ->house_num_list t (((2,8), house_num_padding houses)::acc)   
    | 29 ->house_num_list t (((2,10), house_num_padding houses)::acc) 
    | 31 ->house_num_list t (((5,11), house_num_padding houses)::acc)  
    | 32 ->house_num_list t (((8,11), house_num_padding houses)::acc) 
    | 34 ->house_num_list t (((14,11), house_num_padding houses)::acc) 
    | 37 ->house_num_list t (((23,11), house_num_padding houses)::acc) 
    | 39 ->house_num_list t (((29,11), house_num_padding houses)::acc)

    | 5 ->house_num_list t (((32,6), house_num_padding houses)::acc)
    | 12 ->house_num_list t (((26,1), house_num_padding houses)::acc)
    | 15 ->house_num_list t (((17,1), house_num_padding houses)::acc)
    | 25 ->house_num_list t (((2,6), house_num_padding houses)::acc)
    | 28 ->house_num_list t (((2,9), house_num_padding houses)::acc)
    | 35 ->house_num_list t (((17,11), house_num_padding houses)::acc)
    | n -> house_num_list t acc 



(**[init_board] is the hardcoding of the printable baord in its initial state
   - all spaces are initialized
     VERY MESSY - do not change - all formatting is required*)
let init_board =
  empty_dic 
  (*property names*)
  |> Dic.add (1,1) "|Free   |"   |> Dic.add (1,2) "|Ken Ave|"  
  |> Dic.add (1,3) "|   ?   |"   |> Dic.add (1,4) "|Ind Ave|"  
  |> Dic.add (1,5) "|Ill Ave|"   |> Dic.add (1,6) "|B&O RR |"
  |> Dic.add (1,7) "|Atl Ave|"   |> Dic.add (1,8) "|Ven Ave|"  
  |> Dic.add (1,9) "|Wtr Wrk|"   |> Dic.add (1,10) "|Mar G  |"  
  |> Dic.add (1,11) "|Go 2 J |"  |> Dic.add (4,1) "|NY  Ave|"
  |> Dic.add (7,1) "|Ten Ave|"   |> Dic.add (10,1) "|Comm Ch|"  
  |> Dic.add (13,1) "|St. Jms|"  |> Dic.add (16,1) "|Penn RR|"   
  |> Dic.add (19,1) "|VA  Ave|"  |> Dic.add (22,1) "|State  |"
  |> Dic.add (25,1) "|Ele C  |"  |> Dic.add (28,1) "|St. Ch |" 
  |> Dic.add (31,1) "|Jail   |"  |> Dic.add (31,2) "|Con Ave|"  
  |> Dic.add (31,3) "|VT  Ave|"  |> Dic.add (31,4) "|   ?   |"
  |> Dic.add (31,5) "|Ori Ave|"  |> Dic.add (31,6) "|Read RR|"
  |> Dic.add (31,7) "|Inc Tax|"  |> Dic.add (31,8) "|Bal Ave|"  
  |> Dic.add (31,9) "|Comm Ch|"  |> Dic.add (31,10) "|Med Ave|"
  |> Dic.add (31,11) "|  G O  |" |> Dic.add (4,11) "|Pac Ave|"
  |> Dic.add (7,11) "|NC  Ave|"  |> Dic.add (10,11) "|Comm C |" 
  |> Dic.add (13,11) "|Pen Ave|" |> Dic.add (16,11) "|Short  |"
  |> Dic.add (19,11) "|   ?   |" |> Dic.add (22,11) "|Park Pl|" 
  |> Dic.add (25,11) "|Lux Tax|" |> Dic.add (28,11) "|Board W|" 
  (*houses - place holder for now*)
  |> Dic.add (2,1) "| Park  |"   |> Dic.add (2,2) "|       |" 
  |> Dic.add (2,3) "|   ?   |"   |> Dic.add (2,4) "|       |"  
  |> Dic.add (2,5) "|       |"   |> Dic.add (2,6) "|#######|"
  |> Dic.add (2,7) "|       |"   |> Dic.add (2,8) "|       |"  
  |> Dic.add (2,9) "|  ≈≈≈  |"   |> Dic.add (2,10) "|       |"  
  |> Dic.add (2,11) "|||||||||"  |> Dic.add (5,1) "|       |"
  |> Dic.add (8,1) "|       |"   |> Dic.add (11,1) "|       |"  
  |> Dic.add (14,1) "|       |"  |> Dic.add (17,1) "|#######|"   
  |> Dic.add (20,1) "|       |"  |> Dic.add (23,1) "|       |"
  |> Dic.add (26,1) "|  ϟϟϟ  |"  |> Dic.add (29,1) "|       |" 
  |> Dic.add (32,1) "|||||||||"  |> Dic.add (32,2) "|       |"  
  |> Dic.add (32,3) "|       |"  |> Dic.add (32,4) "|   ?   |"
  |> Dic.add (32,5) "|       |"  |> Dic.add (32,6) "|#######|" 
  |> Dic.add (32,7) "|       |"  |> Dic.add (32,8) "|       |"  
  |> Dic.add (32,9) "|       |"  |> Dic.add (32,10) "|       |"
  |> Dic.add (32,11) "|       |" |> Dic.add (5,11) "|       |" 
  |> Dic.add (8,11) "|       |"  |> Dic.add (11,11) "|       |"  
  |> Dic.add (14,11) "|       |" |> Dic.add (17,11) "|#######|"
  |> Dic.add (20,11) "|   ?   |" |> Dic.add (23,11) "|       |" 
  |> Dic.add (26,11) "|       |" |> Dic.add (29,11) "|       |" 
  (*name spaces - when no player is on them*)
  |> Dic.add (3,1) "|       |"    |> Dic.add (3,2) "|       |" 
  |> Dic.add (3,3) "|       |"    |> Dic.add (3,4) "|       |"   
  |> Dic.add (3,5) "|       |"    |> Dic.add (3,6) "|       |"
  |> Dic.add (3,7) "|       |"    |> Dic.add (3,8) "|       |"  
  |> Dic.add (3,9) "|       |"    |> Dic.add (3,10) "|       |"  
  |> Dic.add (3,11) "|       |"   |> Dic.add (6,1) "|       |"
  |> Dic.add (9,1) "|       |"    |> Dic.add (12,1) "|       |" 
  |> Dic.add (15,1) "|       |"   |> Dic.add (18,1) "|       |"   
  |> Dic.add (21,1) "|       |"   |> Dic.add (24,1) "|       |"
  |> Dic.add (27,1) "|       |"   |> Dic.add (30,1) "|       |" 
  |> Dic.add (33,1) "|       |"   |> Dic.add (33,2) "|       |"  
  |> Dic.add (33,3) "|       |"   |> Dic.add (33,4) "|       |"
  |> Dic.add (33,5) "|       |"   |> Dic.add (33,6) "|       |" 
  |> Dic.add (33,7) "|       |"   |> Dic.add (33,8) "|       |"  
  |> Dic.add (33,9) "|       |"   |> Dic.add (33,10) "|       |"
  |> Dic.add (33,11) "|       |"  |> Dic.add (6,11) "|       |"
  |> Dic.add (9,11) "|       |"   |> Dic.add (12,11) "|       |" 
  |> Dic.add (15,11) "|       |"  |> Dic.add (18,11) "|       |"
  |> Dic.add (21,11) "|       |"  |> Dic.add (24,11) "|       |"
  |> Dic.add (27,11) "|       |"  |> Dic.add (30,11) "|       |" 
  (* Middle of the board*)
  |> Dic.add (4,6) "Rolling Phase:" |> Dic.add (4,7) "" 
  |> Dic.add (4,8) "roll, mortgage/unmortgage," |> Dic.add (4,9) "" 
  |> Dic.add (4,10) "     " 
  |> Dic.add (5,7) "buy/sell #" |> Dic.add (5,8) "houses on" 
  |> Dic.add (5,9) "property," |> Dic.add (5,10) "        "
  |> Dic.add (6,7) "trade with <player>   " |> Dic.add (6,8) "" 
  |> Dic.add (6,9) "     "

  |> Dic.add (7,6) "Landing Buy Phase:" |> Dic.add (7,7) "" 
  |> Dic.add (7,8) "buy, pass, mortgage," |> Dic.add (7,9) "" 
  |> Dic.add (7,10) "       "
  |> Dic.add (8,7) "sell # houses on property," |> Dic.add (8,8) ""
  |> Dic.add (8,9) " " 
  |> Dic.add (9,7) "                      " |> Dic.add (9,8) "" 
  |> Dic.add (9,9) "     "

  |> Dic.add (10,6) "Landing Pay Phase:" 
  |> Dic.add (10,7) "pay, declare bankruptcy," |> Dic.add (10,8) "   " 
  |> Dic.add (10,9) "" |> Dic.add (10,10) ""
  |> Dic.add (11,7) "sell # houses on property " |> Dic.add (11,8) "" 
  |> Dic.add (11,9) " "
  |> Dic.add (12,7) "check money/properties, mortgage" |> Dic.add (12,8) "" 
  |> Dic.add (12,9) "" |> Dic.add (12,10) "    "

  |> Dic.add (13,6) "Auction Phase:" |> Dic.add (13,7) "" 
  |> Dic.add (13,8) "No commands-triggered by" |> Dic.add (13,9) "" 
  |> Dic.add (13,10) "       "
  |> Dic.add (14,7) "      passing in landing phase" |> Dic.add (14,8) "      " 
  |> Dic.add (14,9) "" |> Dic.add (14,10) ""
  |> Dic.add (15,7) "         " |> Dic.add (15,8) "         " 
  |> Dic.add (15,9) "         "

  |> Dic.add (16,6) "Trade Phase:" |> Dic.add (16,7) "" 
  |> Dic.add (16,8) "trade x for y, cancel" |> Dic.add (16,9) "   "
  |> Dic.add (17,7) "(x,y can be cash or a property) " |> Dic.add (17,8) "" 
  |> Dic.add (17,9) "" |> Dic.add (17,10) "    "
  |> Dic.add (18,7) "         " |> Dic.add (18,8) "         " 
  |> Dic.add (18,9) "         "

  |> Dic.add (19,6) "Final Phase:" 
  |> Dic.add (19,7) "end turn, mortgage/unmortgage," |> Dic.add (19,8) "" 
  |> Dic.add (19,9) ""|> Dic.add (19,10) "   "
  |> Dic.add (20,7) "buy/sell # houses on property," |> Dic.add (20,8) ""
  |> Dic.add (20,9) "" |> Dic.add (20,10) "      "
  |> Dic.add (21,7) "trade with <player>" |> Dic.add (21,8) "" 
  |> Dic.add (21,9) "        "

  |> Dic.add (22,6) "Anytime: " |> Dic.add (22,7) "quit, check money/properties"
  |> Dic.add (22,8) "" |> Dic.add(22,9) "" |> Dic.add (22,10) "        "
  (*top let of middle board *)

  (* |> Dic.add (4,3) "Player:  "   
     |> Dic.add (6,3) "Money:   "   *)

  (*Monopoly sign *)
  |> Dic.add(7,3) "| |  | | " |> Dic.add(8,3) "|  ||  | " 
  |> Dic.add(9,3) "|      | "
  |> Dic.add(10,4) " ------  " |> Dic.add(11,4) "|      | " 
  |> Dic.add(12,4) " ------  "
  |> Dic.add(13,3) "||     | " |> Dic.add(14,3) "|   |  | " 
  |> Dic.add(15,3) "|     || "
  |> Dic.add(16,4) " ------  " |> Dic.add(17,4) "|      | " 
  |> Dic.add(18,4) " ------  "
  |> Dic.add(19,3) " ------  " |> Dic.add(20,3) "| _ _ _| " 
  |> Dic.add(21,3) "||       "
  |> Dic.add(22,4) " ------  " |> Dic.add(23,4) "|      | " 
  |> Dic.add(24,4) " ------  "
  |> Dic.add(25,3) "||       " |> Dic.add(26,3) "||       " 
  |> Dic.add(27,3) "|| _ _ _ "
  |> Dic.add(28,4) "||    || " |> Dic.add(29,4) "   ||    " 
  |> Dic.add(30,4) "   ||    "

  (*Signatures *)
  |> Dic.add (28,6) "Authors: " |> Dic.add (28,7) "Haashim Shah" 
  |> Dic.add (28,8) "      "
  |> Dic.add (29,7) "Nicolas Vega" |> Dic.add (29,8) "      "
  |> Dic.add (30,7) "Matthew Simon" |> Dic.add (30,8) "     "


(**[color_print i j dic] prints a string obtained from [dic] formatted with
   color to location [i],[j] in the matrix being printed to the terminal. *)
let color_print i j dic =
  match i,j with 
  (*prop names *)
  | (1,1) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" "); 
  | (1,2) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (1,3) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (1,4) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (1,5) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (1,6) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (1,7) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (1,8) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (1,9) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (1,10) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (1,11) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 

  | (4,1) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (7,1) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" "); 
  | (10,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (13,1) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (16,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (19,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (22,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (25,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (28,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,1) -> ANSITerminal.(print_string [on_black;white] 
                              (Dic.find (i,j) dic)); print_string(" ");

  | (31,2) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,3) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,4) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (31,5) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,6) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (31,7) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (31,8) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,9) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (31,10) -> ANSITerminal.(print_string [on_green;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (31,11) -> ANSITerminal.(print_string [on_red;black] 
                               (Dic.find (i,j) dic)); print_string(" ");

  | (4,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (7,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (10,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (13,11) -> ANSITerminal.(print_string [on_green;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (16,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (19,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (22,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (25,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (28,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" ");


    (*houses*)
  | (2,1) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,2) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,3) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (2,4) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,5) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,6) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (2,7) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,8) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (2,9) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (2,10) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (2,11) -> ANSITerminal.(print_string [on_black;white] 
                              (Dic.find (i,j) dic)); print_string(" ");

  | (5,1) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (8,1) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" "); 
  | (11,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (14,1) -> ANSITerminal.(print_string [on_red;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (17,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (20,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (23,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (26,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (29,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (32,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic )) ; print_string(" ");

  | (32,2) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (32,3) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (32,4) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (32,5) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (32,6) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (32,7) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (32,8) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (32,9) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (32,10) -> ANSITerminal.(print_string [on_yellow;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (32,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");

  | (5,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (8,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (11,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (14,11) -> ANSITerminal.(print_string [on_green;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (17,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (20,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (23,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (26,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (29,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" ");

    (*player names*)
  | (3,1) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,2) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,3) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (3,4) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,5) -> ANSITerminal.(print_string [on_red;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,6) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (3,7) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,8) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");  
  | (3,9) -> ANSITerminal.(print_string [on_white;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (3,10) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (3,11) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");

  | (6,1) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" ");
  | (9,1) -> ANSITerminal.(print_string [on_yellow;black] 
                             (Dic.find (i,j) dic)); print_string(" "); 
  | (12,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (15,1) -> ANSITerminal.(print_string [on_yellow;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (18,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (21,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (24,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (27,1) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");  
  | (30,1) -> ANSITerminal.(print_string [on_magenta;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 

  | (33,1) -> ANSITerminal.(print_string [on_black;white] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (33,2) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (33,3) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (33,4) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (33,5) -> ANSITerminal.(print_string [on_cyan;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (33,6) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (33,7) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (33,8) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (33,9) -> ANSITerminal.(print_string [on_white;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (33,10) -> ANSITerminal.(print_string [on_green;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (33,11) -> ANSITerminal.(print_string [on_red;black] 
                               (Dic.find (i,j) dic)); print_string(" ");

  | (6,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" "); 
  | (9,11) -> ANSITerminal.(print_string [on_green;black] 
                              (Dic.find (i,j) dic)); print_string(" ");
  | (12,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (15,11) -> ANSITerminal.(print_string [on_green;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (18,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (21,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (24,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" "); 
  | (27,11) -> ANSITerminal.(print_string [on_white;black] 
                               (Dic.find (i,j) dic)); print_string(" ");
  | (30,11) -> ANSITerminal.(print_string [on_blue;black] 
                               (Dic.find (i,j) dic)); print_string(" ");

    (*Middle of the baord*)
  | n, m -> 
    if (n<31 && n>3) 
    then (
      if(m>4 && m<11)
      then (ANSITerminal.(print_string [white] 
                            (Dic.find (i,j) dic)); print_string(" ");) 
      else (ANSITerminal.(print_string [green;Bold](Dic.find (i,j) dic)); 
            print_string(" ");)) 
    else (failwith "cant happen")


(**[print_board dic] prints the board formamted as a matrix of strings to the
    terminal.  The string to be printed is determined by using the location 
    (i,j) in the matrix and finding the corresponding 
    (i,j) key within dic. *)
let print_board dic = 
  print_newline(); print_newline(); print_newline(); 
  for i = 1 to 33 do 
    for j = 1 to 11 do 
      if (Dic.mem (i,j) dic) then (color_print i j dic )
      else (ANSITerminal.(print_string [green] ("          ")))
    done;
    print_newline(); if (i mod 3 = 0) then (print_newline();) else (())
  done   

(** [update_board init_board updated_board_entries] adds all key values pairs 
     from [updated_board_entries] to [init_board] this represents the hardcoded
     initial board - with the updated moves added ot it*)
let rec update_board init_board updated_board_entries = 
  match updated_board_entries with
  | [] -> init_board
  | (k,v)::t -> update_board (Dic.add k v init_board) t 


let print_final_board (state : State.t) = 
  print_board (update_board init_board 
                 ((house_num_list (get_house_mortgage_for_board state) []) @ 
                  (player_loc_list (get_all_players state) [])));
  (* @ 
     [((6,4),string_of_int(get_money (get_current_player state)));
     ((4,4),name_padded(Player.get_name (get_current_player state)))])) *)


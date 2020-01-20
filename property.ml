(* TYPE DEFINITIONS AND TYPE MAKER FUNCTIONS *)
type color = 
  | Purple 
  | LightBlue 
  | Pink 
  | Orange 
  | Red 
  | Yellow 
  | Green 
  | DarkBlue 


type colored_prop_info = 
  {name : string; price : int; is_mortgaged : bool; owner : int option; 
   num_houses : int; rent_prices : int list; build_costs : int; color : color}

let make_colored_prop_info name price is_mortgaged owner num_houses rent_prices
    build_costs color = {name = name; price = price; is_mortgaged = is_mortgaged;
                         owner = owner; num_houses = num_houses; 
                         rent_prices = rent_prices; 
                         build_costs = build_costs; color = color}

type rr_prop_info = {name : string; price : int; is_mortgaged : bool;
                     owner : int option; rent_prices : int list}

let make_rr_prop_info name price is_mortgaged owner rent_prices =
  {name = name; price = price; is_mortgaged = is_mortgaged; owner = owner;
   rent_prices = rent_prices}

type utility_prop_info = {name : string; price : int; is_mortgaged : bool;
                          owner : int option; rent_multipliers : int list}

let make_utility_prop_info name price is_mortgaged owner rent_multipliers = 
  {name = name; price = price; is_mortgaged = is_mortgaged; owner = owner;
   rent_multipliers = rent_multipliers}

type commercial_type = 
  | Colored of colored_prop_info
  | Railroad of rr_prop_info
  | Utility of utility_prop_info

type non_commercial_type = 
  | Go of string * int (* carries how much money one recieves for passing go *)
  | Tax of string * int (* carries how much money one needs to pay on the tax space *)
  | GoJail of string * int (* carries the fine *)
  | Jail of string 
  | CCC of string
  | FreeParking of string

type t = 
  | Commercial of commercial_type
  | NonCommercial of non_commercial_type

(* FUNCTIONS IN INTERFACE *)
let set_owner player_index = function
  | Commercial ct -> 
    (match ct with
     | Colored i -> Commercial (Colored {i with owner = Some player_index})
     | Railroad i -> Commercial (Railroad {i with owner = Some player_index})
     | Utility i -> Commercial (Utility {i with owner = Some player_index}))
  | NonCommercial _ -> failwith "Don't set owner on a non-commercial property"


let get_color = function
  | Commercial ct -> 
    (match ct with
     | Colored i -> i.color
     | _ -> failwith "Don't check the color of a non colored property")
  | NonCommercial _ -> failwith "Don't check the color of a non-commercial 
  property"

let get_owner = function
  | Commercial ct ->
    (match ct with
     | Colored i -> i.owner
     | Railroad i -> i.owner
     | Utility i -> i.owner)
  | NonCommercial _ -> failwith "Don't get owner of a non-commercial property"

let is_commercial = function
  | Commercial _ -> true
  | NonCommercial _ -> false

let property_is_mortgaged = function
  | Commercial ct -> 
    (match ct with
     | Colored i -> i.is_mortgaged
     | Railroad i -> i.is_mortgaged
     | Utility i -> i.is_mortgaged)
  | NonCommercial _ -> failwith "is_mortgaged precondition violated"


let reset_property property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       Commercial (Colored 
                     {info with is_mortgaged = false;
                                num_houses = 0; 
                                owner = None})
     | Railroad info -> 
       Commercial (Railroad {info with is_mortgaged = false;
                                       owner = None})
     | Utility info -> 
       Commercial (Utility {info with is_mortgaged = false; 
                                      owner = None}))
  | NonCommercial _ -> failwith "reset_properties precondition violated"

let buy_houses num property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       if num + info.num_houses > 5 
       then failwith "buy_houses preconditin violated"
       else Commercial (Colored {info with num_houses = info.num_houses + num})
     | _ -> failwith "buy_houses preconditin violated")
  | NonCommercial _ -> failwith "buy_houses precondition violated"


let get_price = function
  | Commercial ct -> 
    (match ct with
     | Colored info -> info.price
     | Railroad info -> info.price
     | Utility info -> info.price)
  | NonCommercial _ -> failwith "get_price precondition violated"

let get_name = function
  | Commercial ct ->
    (match ct with
     | Colored info -> 
       info.name 
     | Railroad info -> 
       info.name 
     | Utility info -> 
       info.name)
  | NonCommercial nct -> 
    (match nct with
     | Go (name, _) -> name
     | Tax (name, _) -> name
     | GoJail (name, _) -> name
     | Jail name -> name
     | CCC name -> name
     | FreeParking name -> name)

let sell_houses num = function
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       if not info.is_mortgaged && info.num_houses >= num 
       then 
         Commercial (Colored {info with num_houses = info.num_houses - num})
       else failwith "sell_houses precondition violated"
     | _ -> failwith "sell_houses precondition violated")
  | NonCommercial _ -> failwith "sell_houses precondition violated"

let set_mortgaged = function 
  | Commercial ct -> 
    (match ct with
     | Colored i -> 
       if not i.is_mortgaged && i.num_houses = 0
       then Commercial (Colored {i with is_mortgaged = true})
       else failwith "set_mortgaged precondition violated"
     | Railroad i -> 
       if not i.is_mortgaged then Commercial (Railroad {i with is_mortgaged = true})
       else failwith "set_mortgaged precondition violated"
     | Utility i -> 
       if not i.is_mortgaged then Commercial (Utility {i with is_mortgaged = true})
       else failwith "set_mortgaged precondition violated")
  | NonCommercial _ -> failwith "set_mortgaged precondition violated"

let set_unmortgaged = function
  | Commercial ct -> 
    (match ct with 
     | Colored i -> 
       if i.is_mortgaged then Commercial (Colored {i with is_mortgaged = false})
       else failwith "set_unmortgaged precondition violated" 
     | Railroad i -> 
       if i.is_mortgaged then Commercial (Railroad {i with is_mortgaged = false})
       else failwith "set_unmortgaged precondition violated" 
     | Utility i -> 
       if i.is_mortgaged then Commercial (Utility {i with is_mortgaged = false})
       else failwith "set_unmortgaged precondition violated" )
  | NonCommercial _ -> failwith "set_unmortgaged precondition violated"

let get_houses = function
  | Commercial ct -> 
    (match ct with
     | Colored i -> i.num_houses
     | _ -> failwith "get_houses precondition violated")
  | NonCommercial _ -> failwith "get_houses precondition violated" 

let is_colored = function
  | Commercial ct -> 
    (match ct with
     | Colored _ -> true
     | _ -> false)
  | NonCommercial _ -> false

(* PROPERTY DEFINITIONS FOR STANDARD MONOPOLY GAME IN INITIAL STATE  
   Changing these values change the initial state. Can be useful for play testing,
   but make sure that you save your changes*)
let go = NonCommercial (Go ("Go", Consts.go_money))


let mediterranean_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Mediterranean Avenue" 60 
                   false None 0 [2;10;30;90;160;250] 50 Purple))

let baltic_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Baltic Avenue" 80 
                   false None 0 [4;20;60;180;320;450] 50 Purple))


let oriental_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Oriental Avenue" 100
                   false None 0 [6;30;90;270;400;550] 50 LightBlue))


let vermont_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Vermont Avenue" 100
                   false None 0 [6;30;90;270;400;550] 50 LightBlue))


let connecticut_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Connecticut Avenue" 120
                   false None 0 [8;40;100;300;450;600] 50 LightBlue))


let st_james = 
  Commercial (Colored 
                (make_colored_prop_info "St.James Place" 180
                   false None 0 [14;70;200;550;750;950] 100 Orange))


let tenn_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Tennessee Avenue" 180
                   false None 0 [14;70;200;550;750;950] 100 Orange))


let ny_ave = 
  Commercial (Colored 
                (make_colored_prop_info "New York Avenue" 200
                   false None 0 [16;80;220;600;800;1000] 100 Orange))


let st_charles = 
  Commercial (Colored 
                (make_colored_prop_info "St.Charles Avenue" 140
                   false None 0 [10;50;150;450;625;750] 100 Pink))


let states_ave = 
  Commercial (Colored 
                (make_colored_prop_info "States Avenue" 140
                   false None 0 [10;50;150;450;625;750] 100 Pink))


let virginia_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Virginia Avenue" 160
                   false None 0 [12;60;180;500;700;900] 100 Pink))


let ventnor_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Ventnor Avenue" 260
                   false None 0 [22;110;330;800;975;1150] 150 Yellow))


let atl_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Atlantic Avenue" 260
                   false None 0 [22;110;330;800;975;1150] 150 Yellow))


let marv_gardens = 
  Commercial (Colored 
                (make_colored_prop_info "Marvin Gardens" 280
                   false None 0 [24;120;360;850;1025;1200] 150 Yellow))



let kn_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Kentucky Avenue" 220
                   false None 0 [18;90;250;700;875;1050] 150 Red))


let ind_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Indiana Avenue" 220
                   false None 0 [18;90;250;700;875;1050] 150 Red))


let ill_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Illinois Avenue" 240
                   false None 0 [20;100;300;750;925;110] 150 Red))


let pacific_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Pacific Avenue" 300
                   false None 0 [26;130;390;900;1100;1275] 200 Green))


let ncar_ave = 
  Commercial (Colored 
                (make_colored_prop_info "North Carolina Avenue" 300
                   false None 0 [26;130;390;900;1100;1275] 200 Green))


let penn_ave = 
  Commercial (Colored 
                (make_colored_prop_info "Pennsylvania Avenue" 320
                   false None 0 [28;150;450;1000;1200;1400] 200 Green))


let park_place = 
  Commercial (Colored 
                (make_colored_prop_info "Park Place" 350
                   false None 0 [35;175;500;1100;1300;1500] 200 DarkBlue))


let boardwalk = 
  Commercial (Colored 
                (make_colored_prop_info "Boardwalk" 400
                   false None 0 [50;200;600;1400;1700;2000] 200 DarkBlue))

let reading_railroad = 
  Commercial (Railroad 
                (make_rr_prop_info "Reading Railroad" 200 false None 
                   [25;50;100;200]))

let bo_railroad = 
  Commercial (Railroad 
                (make_rr_prop_info "B&O Railroad" 200 false None 
                   [25;50;100;200]))

let penn_railroad = 
  Commercial (Railroad 
                (make_rr_prop_info "Pennsylvania Railroad" 200 false None 
                   [25;50;100;200]))

let sl_railroad = 
  Commercial (Railroad 
                (make_rr_prop_info "Short Line Railroad" 200 false None 
                   [25;50;100;200]))

let electric_comp = 
  Commercial (Utility 
                (make_utility_prop_info "Electric Company" 150 false None 
                   [4; 10]))

let water_works = 
  Commercial (Utility 
                (make_utility_prop_info "Water Works" 150 false None [4; 10]))


let comm_chest = 
  NonCommercial (CCC "Communnity Chest")

let chance = NonCommercial (CCC "Chance")

let go_jail = NonCommercial (GoJail ("Go To Jail", Consts.jail_money))

let jail = NonCommercial (Jail "Jail")

let free_parking = NonCommercial (FreeParking "Free Parking")

let tax = NonCommercial (Tax ("Tax", Consts.tax_money))
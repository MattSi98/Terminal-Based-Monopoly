open Property


let properties_of_same_color properties property =
  match property with
  | Commercial ct ->
    (match ct with
     | Colored i -> 
       List.filter 
         (fun p -> try p |> Property.get_color = i.color with _ -> false)
         properties
     | _ -> failwith "don't get properties of the same color as a non color 
     property")
  | NonCommercial _ -> failwith "don't get properties of the same color as a non
  commercial property"


let no_houses_on_color property properties = 
  match property with 
  | Commercial ct -> 
    (match ct with
     | Colored _ -> property |> properties_of_same_color properties |> List.for_all
                      (fun p -> p |> get_houses = 0)
     | _ -> true)
  | NonCommercial _ -> true

(** [properties_have_same_owner properties] is true if every property in [properties]
    has the same owner and that owner is a person, not the Bank. False otherwise
    Requires: every property in [properties] is a commercial property and
    properties is not an empty list
    Raises: Failue if precondition is violated*)
let properties_have_same_owner properties = 
  if List.for_all (fun p -> is_commercial p) properties 
  then 
    fst (List.fold_left 
           (* left_solution is (bool, owner) *)
           (fun left_solution p -> 
              if fst left_solution then (get_owner p = snd left_solution, 
                                         snd left_solution)
              else left_solution) 

           (List.hd properties |> get_owner <> None, List.hd properties |> get_owner)

           (match properties with 
            | [] -> failwith "properties_have_same_owners precondition violated"
            | h::t -> t))
  else 
    failwith "properties_have_same_owners precondition violated"

(** [properties_have_zero_houses properties] is true if all the [properties] have
    zero houses, false otherwise. 
    Requires: all the [properties] are colored properties
    Raises: Failure if precondition is violated*)
let properties_have_zero_houses properties = 
  List.for_all (fun p -> 
      match p with 
      | Commercial ct -> 
        (match ct with
         | Colored i -> i.num_houses = 0
         | _ -> failwith "properties_have_zero_houses precondtion violated")
      | NonCommercial _ -> failwith "properties_have_zero_houses precondtion 
      violated")
    properties

(** [properties_not_mortgaged properties] is true if all the [properties] are
    not mortgaged. False otherwise.
    Requires : all the [properties] are commercial properties
    Raises: Failure if precondition is violated*)
let properties_not_mortgaged properties = 
  List.for_all (fun p -> 
      match p with
      | Commercial ct -> not (p |> property_is_mortgaged)
      | NonCommercial _ -> failwith "properties_not_mortgaged precondition violated")
    properties

(** [extract_utilities properties] is a list of the utility properties in 
    [properties] *)
let extract_utilities properties = 
  List.filter (fun p -> 
      match p with 
      | Commercial ct -> 
        (match ct with
         | Utility _ -> true
         | _ -> false)
      | NonCommercial _ -> false) properties

(** [extract_unmortgaged properties] is a list of all the unmortgaged 
    properties in [properties]
    Requires: every property in [properties] is commercial
    Raises: Failure if precondtion is violated*)
let extract_unmortgaged properties =
  List.filter (fun p -> 
      match p with 
      | Commercial ct -> not (p |> property_is_mortgaged)
      | NonCommercial _ -> failwith "extract_unmortgaged precondition violated")
    properties

(** [extract_railroads properties] is a list of the railroad properties in 
    [properties] *)
let extract_railroads properties = 
  List.filter (fun p -> 
      match p with 
      | Commercial ct -> 
        (match ct with
         | Railroad _ -> true
         | _ -> false)
      | NonCommercial _ -> false) properties

let check_invs (properties:t list) = 
  (* checking invariant 1*)
  if (List.for_all (fun p -> 
      match p with
      | Commercial ct -> 
        (match ct with
         | Colored i -> 
           if i.num_houses > 0 
           then let properties' = properties_of_same_color properties p in
             properties' |> properties_have_same_owner &&
             properties' |> properties_not_mortgaged
           else true
         | _ -> true)
      | NonCommercial _ -> true) properties) &&

     (* checking invariant 2 *)
     (List.for_all (fun p -> 
          match p with
          | Commercial ct -> 
            (match p |> get_owner with
             | Some index -> true
             | None -> 
               (match ct with
                | Colored info -> not info.is_mortgaged && info.num_houses = 0
                | Railroad info -> not info.is_mortgaged
                | Utility info -> not info.is_mortgaged))
          | NonCommercial _ -> true) properties)

  then properties
  else failwith "properties invariants broken"


let make_properties_list () = 
  [go; mediterranean_ave; comm_chest; baltic_ave; tax; reading_railroad;
   oriental_ave; chance; vermont_ave; connecticut_ave; jail; st_charles; electric_comp;
   states_ave; virginia_ave; penn_railroad; st_james; comm_chest; tenn_ave; ny_ave;
   free_parking; kn_ave; chance; ind_ave; ill_ave; bo_railroad; atl_ave; ventnor_ave; 
   water_works; marv_gardens; go_jail; pacific_ave; ncar_ave; comm_chest; penn_ave; 
   sl_railroad; chance; park_place; tax; boardwalk] 


let get_player_properties player_index properties = 
  List.fold_left 
    (fun left_solution p -> 
       match p with
       | Commercial ct -> 
         (match p |> get_owner with
          | Some index -> if index = player_index then p :: left_solution 
            else left_solution
          | None -> left_solution)
       | NonCommercial _ -> left_solution) [] properties

let can_buy_house player_index properties property = 
  match property with
  | Commercial ct -> 
    (match ct with 
     | Colored info -> 
       (match info.owner with
        | Some index -> 
          if index = player_index then 
            let properties' = property |> properties_of_same_color properties in
            properties' |> properties_have_same_owner &&
            properties' |> properties_not_mortgaged &&
            info.num_houses < 5
          else false
        | None -> false)
     | _ -> failwith "can_buy_house precondition violated")
  | NonCommercial _ -> failwith "can_buy_house precondition violated"



let value_of_assets player_index properties = 
  List.fold_left 
    (fun left_solution p -> 
       match p with
       | Commercial ct -> 
         (match ct with
          | Colored info -> 
            (match info.owner with
             | Some index -> 
               if index = player_index then 
                 if info.is_mortgaged then left_solution 
                 else 
                   ((info.price / 2) + 
                    ((info.num_houses * info.build_costs) / 2) +
                    left_solution)
               else left_solution
             | None -> left_solution)
          | Railroad info -> 
            (match info.owner with
             | Some index -> 
               if index = player_index then
                 if info.is_mortgaged then left_solution
                 else ((info.price / 2) + left_solution)
               else left_solution
             | None -> left_solution)
          | Utility info -> 
            (match info.owner with
             | Some index -> 
               if index = player_index then
                 if info.is_mortgaged then left_solution
                 else ((info.price / 2) + left_solution)
               else left_solution
             | None -> left_solution))

       | NonCommercial _ -> left_solution) 0 properties

let can_mortgage player_index properties property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Colored info -> 
       (match info.owner with
        | Some index -> 
          if index <> player_index || info.is_mortgaged || info.num_houses > 0
          then false
          else properties_of_same_color properties property |> 
               properties_have_zero_houses
        | None -> false)

     | Railroad info -> 
       (match info.owner with
        | Some index -> index = player_index && not info.is_mortgaged
        | None -> false)
     | Utility info -> 
       (match info.owner with
        | Some index -> index = player_index && not info.is_mortgaged
        | None -> false))

  | NonCommercial _ -> failwith "can_mortgage precondition violated"


(** [colored_rent properties property] is the amount of money one would have to pay
    when landing on [property] given that they don't own it and another player does. 
    Requires: [property] is a colored property and [property] is owned by a player 
    Raises: Failure if precondition is violated*)
let colored_rent properties property = 
  match property with 
  | Commercial ct ->
    (match ct with
     | Colored info -> 
       (match info.owner with
        | Some _ -> 
          if info.is_mortgaged then 0 else List.nth info.rent_prices 
              info.num_houses
        | None -> failwith "colored_rent precondtion violated")
     | _ -> failwith "colored_rent precondtion violated")

  | NonCommercial _ -> failwith "colored_rent precondition violated"


(* The length of the list produced in the next two functions must be at
   least one so subtracting 1 is safe*)

(** [utility_rent properties roll property] is the amount of money one would have to
    pay when landing on [property] given that they don't own it, rolled a [roll]
    to land on it, and another player owns it
    Requires: [property] is a utility property and [property] is owned by a player
    Raises: Failure if precondition is violated*)

let utility_rent properties roll property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Utility info -> 
       (match info.owner with
        | Some index -> 
          if info.is_mortgaged then 0 else 
            ((get_player_properties index properties |> extract_utilities |> 
              extract_unmortgaged |> List.length) - 1 
             |> List.nth info.rent_multipliers)
            * roll
        | None -> failwith "utility_rent precondition vioalted")
     | _ -> failwith "utility_rent precondition vioalted")

  | NonCommercial _ -> failwith "utility_rent precondition vioalted"


(** [railroad_rent properties property] is the amount of money one would have to 
    pay when landing on [property] given that they don't own it, and another player does 
    Requires: [property] is a railroad property and [property] is owned by a player
    Raises: Failure if precondition is violated*)

let railroad_rent properties property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Railroad info -> 
       (match info.owner with
        | Some index -> 
          if info.is_mortgaged then 0 else 
            (get_player_properties index properties |> extract_railroads |>
             extract_unmortgaged |> List.length) - 1 
            |> List.nth info.rent_prices
        | None -> failwith "railroad_rent precondition violated")
     | _ -> failwith "railroad_rent precondition violated")

  | NonCommercial _ -> failwith "railroad_rent precondition violated"


let commercial_rent properties roll property = 
  match property with
  | Commercial ct -> 
    (match ct with
     | Colored _ -> colored_rent properties property
     | Railroad _ -> railroad_rent properties property
     | Utility _ -> utility_rent properties roll property)

  | NonCommercial _ -> failwith "commercial_rent precondition violated"

let get_property_index property properties =
  let rec helper index = function
    | [] -> failwith "get_property_index precondition violated"
    | h::t -> if h = property then index else helper (index + 1) t in
  helper 0 properties
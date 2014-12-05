open Team
open Definitions
open Constants
open Util

let name = 
"my_milkshake_brings_all_the_bots_to_the_yard_damn_right_its_botter_than_yours"

let _ = Random.self_init ()

(* Picking optimal steammon by attack, speed, power of best move, but most
 * importantly, ability to inflict disrupting statuses *)
(* Steammon cost function *)
(* Requires : steammon 
 * Returns : utility of steammon, by the equation 0.25 * att + 0.25 * power +
 * 0.5 * speed *)
let f_steam (s : steammon) : float =
  let moves = [s.first_move; s.second_move; s.third_move; s.fourth_move] in
  let best_move =  List.fold_left 
    (fun a x -> if x.power > a.power then x else a) s.first_move moves in
  let pow = float_of_int best_move.power in
  let att = match best_move.element with
            | Electric
            | Fire
            | Water
            | Grass
            | Ice
            | Dragon
            | Psychic
            | Dark -> float_of_int s.spl_attack
            | _ -> float_of_int s.attack in
  let spe = float_of_int s.speed in
  0.25 *. att +. 0.25 *. pow +. 0.50 *. spe

(* Computes credits remaining for a team of a given color 
 * Requires : game status data and color of team
 * Returns : int of credits remaining *)
let cred_remaining (gsd : game_status_data) (col : color) : int =
  match (col, gsd) with
  | (Red, ((_, _, r_cred), _)) -> r_cred
  | (Blue, (_, (_, _, b_cred))) -> b_cred

(* Computes number credits a steammon pick should be worth. Cost must be
 * below 125% of total credits/number of picks 
 * Requires : game status data and color of team 
 * Returns : int of credits allowed *)
let cred_allowed (gsd : game_status_data) (col : color) : int = 
  min (cred_remaining gsd col)
      (int_of_float ((float_of_int cSTEAMMON_CREDITS) /.
                     (float_of_int cNUM_PICKS) *.
                      1.25))

(* Returns true if the cost of a steammon is under the number of credits 
 * allowed
 * Requires : game status data and color of team and steammon in question
 * Returns : bool *)
let cost_filter (gsd : game_status_data) (col : color) (s : steammon) : bool = 
  s.cost <= cred_allowed gsd col

(* Checks if a move is capable of inflicting paralyzed, asleep, frozen, or
 * confused, returns true if it can
 * Requires : list of effects
 * Returns : bool *)
let has_status (el : effect list) : bool =
  List.fold_left (fun a x -> match x with
                             | InflictStatus Paralyzed
                             | InflictStatus Asleep
                             | InflictStatus Frozen
                             | InflictStatus Confused -> true
                             | _ -> a || false) false el

(* Checks if a steammon is capable of inflicting a status, returns true if it 
 * can 
 * Requires : steammon in question 
 * Returns : bool*)
let status_filter (s : steammon) : bool =
  let moves : move list = 
    [s.first_move; s.second_move; s.third_move; s.fourth_move] in
  let effs : (effect list * int) list = List.map 
    (fun (x : move) -> match x.effects with
                       | None -> ([], 0)
                       | Some (efflst, _, prob) -> (efflst, prob)) moves in
  List.fold_left (fun a (el, p) -> a || (p > 89 && (has_status el))) false effs

(* finds the optimal steammon based on f_steam optimization function 
 * Requires : default steammon and steammon list 
 * Returns : best steammon *)
let find_steam (s0 : steammon) (lst : steammon list) : steammon = 
  List.fold_left (fun a x -> if f_steam x > f_steam a then x else a) s0 lst
                                   
(* Finds best steammon out of all possible steammon. See find_steam
 * Requires : game status data, color, and steam pool 
 * Returns : string (name of steammon to pick)*)
let find_pick (gsd : game_status_data) (col : color) (lst : steammon list) : 
    string =
  let lst' = List.filter (cost_filter gsd col) lst in
  let lst'' = List.filter status_filter lst' in
  let steam = match (lst'', lst', lst) with
              | (h::_, _, _) -> find_steam h lst''
              | ([], h::_, _) -> find_steam h lst'
              | ([], [], h::_) -> find_steam h lst
              | ([], [], []) -> failwith "No steammon to pick" in
  steam.species

(* Picking inventory of MaxPotions *)
let inv : inventory = 
  let n = cINITIAL_CASH / cCOST_MAXPOTION in
  [0; n; 0; 0; 0; 0; 0]

(* Utility functions *)
(* Gets the list of players steammon 
 * Requires : game status data and color 
 * Returns : steammon list *)
let get_list (gsd : game_status_data) (col : color) : steammon list = 
  match (col, gsd) with
  | (Red, ((r_sl, _, _), _)) -> r_sl
  | (Blue, (_, (b_sl, _, _))) -> b_sl

(* Gets the player's active steammon 
 * Requires : game status data and color 
 * Returns : steammon list *)
let get_active (gsd : game_status_data) (col : color) : steammon = 
  match (col, gsd) with
  | (Red, ((rh::_, _, _), _)) -> rh
  | (Blue, (_, (bh::_, _, _))) -> bh
  | _ -> failwith "Invalid steammon lists in get_active"

(* Gets the opponent's active steammon 
 * Requires : game status data and opponent color
 * Returns : steammon *)
let get_opp (gsd : game_status_data) (col : color) : steammon = 
  match (col, gsd) with
  | (Red, (_, (bh::_, _, _))) -> bh
  | (Blue, ((rh::_, _, _), _)) -> rh
  | _ -> failwith "Invalid steammon lists in get_opp"

(* Gets the opponent's list of steammon
 * Requires : game status data and opponent color
 * Returns : steammon list *)
let get_opp_lst (gsd : game_status_data) (col : color) : steammon list =
  match (col, gsd) with
  | (Red, (_, (b_sl, _, _))) -> b_sl
  | (Blue, ((r_sl, _, _), _)) -> r_sl

(* Gets the player's inventory 
 * Requires : game status data and color
 * Returns : inventory *)
let get_inv (gsd : game_status_data) (col : color) : inventory =
  match (col, gsd) with
  | (Red, ((_, r_inv, _), _)) -> r_inv
  | (Blue, (_, (_, b_inv, _))) -> b_inv

(* Comparison function for two steammon by ability to afflict status, then by
 * utilty by f_steam optimization function. Returns positive integer if first
 * steammon is better than second, negative integer if second steammon is
 * better than first, and 0 if they are equal 
 * Requires : two steammon 
 * Returns : int described above *)
let steam_compare (s1 : steammon) (s2 : steammon) : int = 
  if (status_filter s1 && status_filter s2) ||
     ((not (status_filter s1)) && (not (status_filter s2))) 
  then int_of_float ((f_steam s1) -. (f_steam s2))
  else if status_filter s1 then 1
  else -1

(* Returns a sorted list of steammon in decreasing order when compared using
 * steam_compare.
 * Requires : game status data and color
 * Returns : steammon list in decreasing order of utility *)
let order (gsd : game_status_data) (col : color) : steammon list =
  let lst = get_list gsd col in
  List.rev (List.sort steam_compare lst)

(* Returns true if steammon is fainted 
 * Requires : steammon 
 * Returns : bool *)
let is_fainted (s : steammon) : bool = s.curr_hp <= 0

(* Returns true if steammon is asleep or frozen 
 * Requires : steammon 
 * Returns : bool *)
let is_impotent (s : steammon) : bool = 
  match s.status with
  | Some Asleep
  | Some Frozen -> true
  | _ -> false

(* Returns true if opponent has a super effective attack towards the player
 * available
 * Requires : game status data, color, and players active steammon 
 * Returns : bool *)
let is_weak (gsd : game_status_data) (col : color) (s : steammon) : bool =
  let opp = get_opp gsd col in
  let opp_moves = [opp.first_move; opp.second_move; opp.third_move; 
                   opp.fourth_move] in
  let effts = List.map (fun (x : move) -> 
    let (efft, _) = calculate_type_matchup x.element 
                                          (s.first_type, 
                                           s.second_type) in
    efft) opp_moves in
  List.mem SuperEffective effts

(* Returns true if steammon is player has super effective attack towards
 * opponent available
 * Requires : game status data, color, and players active steammon 
 * Returns : bool *)
let is_strong (gsd : game_status_data) (col : color) (s : steammon) : bool =
  let opp = get_opp gsd col in
  let moves = [s.first_move; s.second_move; s.third_move; s.fourth_move] in
  let effts = List.map (fun (x : move) ->
    let (efft, _) = calculate_type_matchup x.element
                                          (opp.first_type,
                                           opp.second_type) in
    efft) moves in
  not (List.mem SuperEffective effts)

(* Filters list of steammon by fainted status, asleep/frozen status, weak
 * status, and then strong status. Returns best possible steammon after
 * filters
 * Requires : game status data and color 
 * Returns : species name of best steammon *)
let filter (gsd : game_status_data) (col : color) : string = 
  let lst = order gsd col in
  let lst1 = List.filter (fun x -> not (is_fainted x)) lst in
  let lst2 = List.filter (fun x -> not (is_impotent x)) lst1 in
  let lst3 = List.filter (fun x -> not ((is_weak gsd col) x)) lst2 in
  let lst4 = List.filter (fun x -> not ((is_strong gsd col) x)) lst3 in
  let steam = match (lst4, lst3, lst2, lst1) with
              | (h::_, _, _, _) -> h
              | ([], h::_, _, _) -> h
              | ([], [], h::_, _) -> h
              | ([], [], [], h::_) -> h
              | ([], [], [], []) -> failwith "All steammon fainted" in
  steam.species

(* list ref keeping track of available max potions for each steammon *)
let mp_lst = ref []

(* finds the number of max potions left in the inventory 
 * Requires : game status data and color 
 * Returns : int of max potions left *)
let n_mps (gsd : game_status_data) (col : color) : int = 
  match get_inv gsd col with
  | 0::n::0::0::0::0::0::[] -> n
  | _ -> failwith "Inventory error"

(* initializes mp_lst 
 * Requires : game status data and color *)
let init_mp_lst (gsd : game_status_data) (col : color) : unit =
  let lst = get_list gsd col in
  let n = n_mps gsd col in
  let nps = if n mod cNUM_PICKS = 0 then n / cNUM_PICKS
            else n / cNUM_PICKS + 1 in
  mp_lst := List.fold_left (fun a x -> (x.species, nps)::a) !mp_lst lst

(* Finds expected damage by a move from attacker to defender 
 * Requires : two steammon and a move 
 * Returns : int of expected damage *)  
let find_dmg (attacker : steammon) (defender : steammon) (mov : move) : int =
  let stab = if List.mem (Some mov.element) [attacker.first_type;
                                             attacker.second_type]
             then cSTAB_BONUS
             else 1. in
  let (efft, s_mult) = calculate_type_matchup mov.element 
                      (defender.first_type, defender.second_type) in
  let bw = match attacker.status with
           | Some Burned -> cBURN_WEAKNESS
           | _ -> 1. in
  let rand = (Random.int(100 - cMIN_DAMAGE_RANGE)) + cMIN_DAMAGE_RANGE in
  let mult = stab *. s_mult *. bw *. (float_of_int rand) /. 100. in
  let (atk, def) = if is_special mov.element 
                   then ((float_of_int attacker.spl_attack) *. 
                   multiplier_of_modifier(attacker.mods).spl_attack_mod,
                   (float_of_int defender.spl_defense) *.
                   multiplier_of_modifier(defender.mods).spl_defense_mod)
                   else ((float_of_int attacker.attack) *.
                   multiplier_of_modifier(attacker.mods).attack_mod,
                   (float_of_int defender.defense) *.
                   multiplier_of_modifier(defender.mods).defense_mod) in
  let (atk, def) = (int_of_float atk, int_of_float def) in
  let pow = mov.power in
  calculate_damage atk def pow mult

(* Decides most effective move from an attacker to a defender 
 * Requires : two steammon 
 * Returns : move *)  
let decide_attack (att : steammon) (def : steammon) : move = 
  let moves = [att.first_move; att.second_move; att.third_move;
  												att.fourth_move] in
  let f = find_dmg att def in
  List.fold_left (fun a (x : move) -> if f x > f a &&
									     x.pp_remaining > 0 
									  then x 
									  else a) att.first_move moves
(* Same as decide attack, although tries to select a move that inflicts a
 * status. If a move like this cannot be found, function resorts to a 
 * default move
 * Requires : default move, two steammon
 * Returns : move *)
let decide_sec_attack (mov : move) (att : steammon) (def : steammon) : move =
  let moves = [att.first_move; att.second_move; att.third_move;
  												att.fourth_move] in
   List.fold_left (fun a (x : move) -> 
					 match x.effects with
					 | Some (el, _, p) when p > 89-> 
						if has_status el then x else a
					 | _ -> a) mov moves

(* Returns true if a move can inflict Frozen 
 * Requires : move 
 * Returns : bool *)
let is_frozen (mov : move) : bool =
  match mov.effects with
  | None -> false
  | Some (el, _, _) -> List.mem (InflictStatus Frozen) el

(* Returns true if a move can inflict Asleep
 * Requires : move
 * Returns : bool *)
let is_asleep (mov : move) : bool = 
  match mov.effects with
  | None -> false
  | Some (el, _, _) -> List.mem (InflictStatus Asleep) el

(* Returns true if anyone on the opposing team is frozen
 * Requires : list of opposing steammon
 * Returns : bool *)
let opp_frozen (opp_lst : steammon list) : bool =
  List.fold_left (fun a x -> a || (x.status = Some Frozen &&
  								   x.curr_hp > 0)) false opp_lst

(* Returns true if anyone on the opposing team is asleep 
 * Requires : list of opposing steammon 
 * Returns : bool*)
let opp_asleep (opp_lst : steammon list) : bool =
  List.fold_left (fun a x -> a || (x.status = Some Asleep &&
  								   x.curr_hp > 0)) false opp_lst

(* Decides action a steammon should take
 * If it is asleep or frozen and has an option to switch to a better steammon,
 * it will switch. If it has low HP and a MaxPotion to use, it will use that
 * If the opponent has no status and has low HP, it will attack with the most
 * effective move. If it have high HP, it will attack with a move that inflicts
 * a status, if it can. If anyone is asleep or frozen on the opposing team and
 * the move that inflicts a status will inflict Frozen or Asleep, it will use
 * the most effective move 
 * Requires : game status data and color
 * Returns : action to take *)
let decide_action (gsd : game_status_data) (col : color) : action = 
  let opp = get_opp gsd col in
  let opp_lst = get_opp_lst gsd col in
  let s = get_active gsd col in
  let attack = decide_attack s opp in
  let sec_attack = decide_sec_attack attack s opp in
  let s' = filter gsd col in
  let n = n_mps gsd col in
  if is_impotent s && s' <> s.species then SwitchSteammon s'
  else if (not (List.mem (s.species, 0) !mp_lst)) && n <> 0 && 
           s.curr_hp < int_of_float ((float_of_int s.max_hp) *. 0.35) 
       then UseItem (MaxPotion, s.species)
  else if opp.status <> None then UseMove attack.name
  else if opp.curr_hp <= find_dmg s opp attack then UseMove attack.name
  else if not (status_filter s) then UseMove attack.name
  else if (is_frozen sec_attack && opp_frozen opp_lst) || 
  		  (is_asleep sec_attack && opp_asleep opp_lst) then UseMove attack.name
  else UseMove sec_attack.name

(* Takes requests from server and returns action to server 
 * Requires : color and request from server 
 * Returns : action for server*)
let handle_request (c : color) (r : request) : action = 
  match r with
  | TeamNameRequest -> SendTeamName name
  | StarterRequest gsd -> SelectStarter (filter gsd c)
  | PickRequest (col, gsd, ms, sp) -> PickSteammon (find_pick gsd col sp)
  | PickInventoryRequest gsd -> init_mp_lst gsd c; PickInventory inv
  | ActionRequest gsd -> decide_action gsd c

(* runs bot *)
let () = run_bot handle_request

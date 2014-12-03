open Definitions
open Util
open Constants
open Netgraphics
open State

(* You have to implement this. Change it from int to yout own state type*)
type game = State.t * game_status_data

let game_datafication g = snd g
	
let game_from_data game_data = (!(State.state), game_data)

let find_cheapest (lst : steammon list) : steammon =
  let first = match lst with 
    | [] -> failwith "Find cheapest list error"
    | h::_ -> h in
  List.fold_left (fun a x -> if x.cost < a.cost then x else a) first lst

let find_awake (lst : steammon list) : steammon = 
  let first = match lst with
    | [] -> failwith "Find awake list error"
    | h::_ -> h in
  List.fold_left (fun a x -> if x.curr_hp > a.curr_hp then x else a) first lst

type status_data = (steammon list * inventory) * (steammon list * inventory)

let initial_effects (sd : status_data) : status_data =
  let r_sl = fst (fst sd) in
  let b_sl = fst (snd sd) in
  match (r_sl, b_sl) with
  | ([], [])
  | ([], _)
  | (_, []) -> failwith "Steammon list error in initial_effects"
  | (rh::rt, bh::bt) -> 
  let mod_status (s : steammon) (col : color) : steammon = 
    let status' = match s.status with
                  | Some Asleep -> if Random.int 99 < cWAKE_UP_CHANCE then 
                                     (add_update (AdditionalEffects 
                                               [(HealedStatus Asleep, col)]); 
                                     None)
                                   else Some Asleep
                  | Some Frozen -> if Random.int 99 < cDEFROST_CHANCE then 
                                     (add_update (AdditionalEffects
                                               [(HealedStatus Frozen, col)]);
                                     None)
                                   else Some Frozen
                  | Some Confused -> if Random.int 99 < cSNAP_OUT_OF_CONFUSION
                                       then (add_update (AdditionalEffects
                                               [(HealedStatus Confused, col)]); 
                                       None)
                                     else Some Confused
                  | x -> x in
    {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
     first_type = s.first_type; second_type = s.second_type;
     first_move = s.first_move; second_move = s.second_move;
     third_move = s.third_move; fourth_move = s.fourth_move;
     attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
     spl_defense = s.spl_defense; speed = s.speed; status = status';
     mods = s.mods; cost = s.cost} in
  let r_sl = (mod_status rh Red) :: rt in
  let b_sl = (mod_status bh Blue) :: bt in
  ((r_sl, snd(fst sd)),(b_sl, snd(snd sd)))

let determine_order (sd : status_data) (r_act : command) (b_act : command) :
    (color * command) * (color * command) =
  match (fst (fst sd), fst (snd sd)) with
  | ([], [])
  | ([], _)
  | (_, []) -> failwith "Steammon list error in determine_order"
  | (rh::_, bh::_) ->
  let true_speed (s : steammon) : float =
    let slow_factor = match s.status with
                      | Some Paralyzed -> float_of_int cPARALYSIS_SLOW
                      | _ -> 1. in
    (float_of_int s.speed) *. 
    (multiplier_of_modifier (s.mods).speed_mod) /.
    slow_factor in
  if (true_speed rh) > (true_speed bh) then (add_update (SetFirstAttacker Red);
                                            ((Red, r_act), (Blue, b_act)))
  else if (true_speed rh) < (true_speed bh) then (
                                            add_update (SetFirstAttacker Blue);
                                            ((Blue, b_act), (Red, r_act)))
  else if Random.int 99 < 50 then (add_update (SetFirstAttacker Red);
                                  ((Red, r_act), (Blue, b_act)))
  else (add_update (SetFirstAttacker Blue); 
       ((Blue, b_act), (Red, r_act)))

let do_action (sd : status_data) (col : color) (act : action) : status_data =
  let sl = match col with
           | Red -> fst (fst sd)
           | Blue -> fst (snd sd) in
  match act with
  | Action (SwitchSteammon str) -> begin
      let s = List.fold_left (fun a x -> if x.species = str then x 
                                         else a) (find_awake sl) sl in
      sl' = s :: (List.filter ((<>) s) sl) in
      add_update (SetChosenSteammon s.species);
      match col with
      | Red -> ((sl, snd (fst sd)), snd sd)
      | Blue -> (fst sd, (sl, snd (snd sd))) end
  | Action (UseItem (itm, str)) -> begin
      let item_pp (s : steammon) : steammon =
        let add_pp (m : move) : move = 
          {name = m.name; element = m.element; target = m.target;
           max_pp = m.max_pp; pp_remaining = min m.max_pp (m.pp_remaining + 5);
           power = m.power; accuracy = m.accuracy; effects = m.effects} in
        let first_move' = add_pp first_move in
        let second_move' = add_pp second_move in
        let third_move' = add_pp third_move in
        let fourth_move' = add_pp fourth_move in
        {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
         first_type = s.first_type; second_type = s.second_type;
         first_move = first_move'; second_move = second_move';
         third_move = third_move'; fourth_move = fourth_move';
         attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
         spl_defense = s.spl_defense; speed = s.speed; status = s.status;
         mods = s.mods; cost = s.cost} in
      let item_hp (s : steammon) (n : int) : steammon =
        {species = s.species; curr_hp = n; max_hp = s.max_hp; 
         first_type = s.first_type; second_type = s.second_type;
         first_move = s.first_move; second_move = s.second_move;
         third_move = s.third_move; fourth_move = s.fourth_move;
         attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
         spl_defense = s.spl_defense; speed = s.speed; status = s.status;
         mods = s.mods; cost = s.cost} in
      let item_status (s : steammon) : steammon = 
        {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
         first_type = s.first_type; second_type = s.second_type;
         first_move = s.first_move; second_move = s.second_move;
         third_move = s.third_move; fourth_move = s.fourth_move;
         attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
         spl_defense = s.spl_defense; speed = s.speed; status = None;
         mods = s.mods; cost = s.cost} in
      let item_mods (s : steammon) (st : stat) : steammon = 
        let mods' = s.mods in
        let mods'' = match st with
                    | SpA
                    | SpD -> failwith "shouldn't be raised"
                    | Atk -> {attack_mod = mods'.attack_mod + 1;
                              defense_mod = mods'.defense_mod;
                              spl_attack_mod = mods'.spl_attack_mod;
                              spl_defense_mod = mods'.spl_defense_mod;
                              speed_mod = mods'.speed_mod}
                    | Def -> {attack_mod = mods'.attack_mod;
                              defense_mod = mods'.defense_mod + 1;
                              spl_attack_mod = mods'.spl_attack_mod;
                              spl_defense_mod = mods'.spl_defense_mod;
                              speed_mod = mods'.speed_mod}
                    | Spe -> {attack_mod = mods'.attack_mod;
                              defense_mod = mods'.defense_mod;
                              spl_attack_mod = mods'.spl_attack_mod;
                              spl_defense_mod = mods'.spl_defense_mod;
                              speed_mod = mods'.speed_mod + 1} in
        {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
         first_type = s.first_type; second_type = s.second_type;
         first_move = s.first_move; second_move = s.second_move;
         third_move = s.third_move; fourth_move = s.fourth_move;
         attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
         spl_defense = s.spl_defense; speed = s.speed; status = s.status;
         mods = mods''; cost = s.cost} in
        match itm with
        | Ether -> failwith "TODO"
        | MaxPotion -> failwith "TODO"
        | Revive -> failwith "TODO"
        | FullHeal -> failwith "TODO"
        | XAttack -> failwith "TODO"
        | XDefense -> failwith "TODO"
        | XSpeed -> failwith "TODO"
  end
  | Action (UseMove str) -> failwith "TODO"
  | _ -> sd

let do_battle (gsd : game_status_data) (r_act : command) (b_act : command) : 
    game_status_data =
  let ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred)) = gsd in
  let sd = ((r_sl, r_inv), (b_sl, b_inv)) in
  let sd = initial_effects sd in
  let ((first, act1), (second, act2)) = determine_order sd r_act b_act in
  let sd = do_action sd first act1 in
  let sd = do_action sd second act2 in
  let sd = after_effects sd in
  ignore first; ignore second; ignore act1; ignore act2;
  let ((r_sl, r_inv), (b_sl, b_inv)) = sd in
  ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred))

let find_next_data (g : game) (ra : command) (ba : command) : 
    game_status_data =
  let curr_data = snd g in
  let (rdata, bdata) = curr_data in
  let ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred)) = (rdata, bdata) in
  let prices = [cCOST_ETHER; cCOST_MAXPOTION; cCOST_REVIVE; cCOST_FULLHEAL;
                cCOST_XATTACK; cCOST_XDEFEND; cCOST_XSPEED] in
  let def_inv = [cNUM_ETHER; cNUM_MAX_POTION; cNUM_REVIVE; cNUM_FULL_HEAL;
                 cNUM_XATTACK; cNUM_XDEFENSE; cNUM_XSPEED] in
  let find_cost a x y = a + (x * y) in
  match (fst g, (ra, ba)) with
  | (Init, (Action (SendTeamName stra), Action (SendTeamName strb))) ->
      send_update (InitGraphics (stra, strb));
      curr_data
  | (Init, (_, Action (SendTeamName strb))) ->
      send_update (InitGraphics ("Red", strb));
      curr_data
  | (Init, (Action (SendTeamName stra), _)) ->
      send_update (InitGraphics (stra, "Blue"));
      curr_data
  | (Init, (_, _)) -> 
      send_update (InitGraphics ("Red", "Blue"));
      curr_data
  | (Draft Red, (Action (PickSteammon stra), _)) -> 
      let sa = Table.find Initialization.mon_table stra in
      let s = if List.mem sa !(State.mon_lst) && r_cred - sa.cost >= 0 then sa
              else find_cheapest !(State.mon_lst) in
      State.mon_lst := List.filter ((<>) s) !(State.mon_lst);
      add_update (UpdateSteammon (s.species, s.curr_hp, s.max_hp, Red));
      if r_cred - s.cost >= 0 then ((s::r_sl, r_inv, r_cred - s.cost), bdata)
      else ((s::r_sl, r_inv, 0), bdata)
  | (Draft Red, (_, _)) -> 
      let s = find_cheapest !(State.mon_lst) in
      add_update (UpdateSteammon (s.species, s.curr_hp, s.max_hp, Red));
      if r_cred - s.cost >= 0 then ((s::r_sl, r_inv, r_cred - s.cost), bdata)
      else ((s::r_sl, r_inv, 0), bdata)
  | (Draft Blue, (_, Action (PickSteammon strb))) ->
      let sb = Table.find Initialization.mon_table strb in
      let s = if List.mem sb !(State.mon_lst) && b_cred - sb.cost >= 0 then sb
              else find_cheapest !(State.mon_lst) in
      State.mon_lst := List.filter ((<>) s) !(State.mon_lst);
      add_update (UpdateSteammon (s.species, s.curr_hp, s.max_hp, Blue));
      if b_cred - s.cost >= 0 then (rdata, (s::b_sl, b_inv, b_cred - s.cost))
      else (rdata, (s::b_sl, b_inv, 0))
  | (Draft Blue, (_, _)) ->
      let s = find_cheapest !(State.mon_lst) in
      add_update (UpdateSteammon (s.species, s.curr_hp, s.max_hp, Blue));
      if b_cred - s.cost >= 0 then (rdata, (s::b_sl, b_inv, b_cred - s.cost))
      else (rdata, (s::b_sl, b_inv, 0))
  | (Buy, (Action (PickInventory inv_r'), Action (PickInventory inv_b'))) ->
      let inv_r'' = 
        if (List.fold_left2 find_cost 0 prices inv_r') <= cINITIAL_CASH then 
          inv_r'
        else def_inv in
      let inv_b'' =
        if (List.fold_left2 find_cost 0 prices inv_b') <= cINITIAL_CASH then
          inv_b'
        else def_inv in
      ((r_sl, inv_r'', r_cred), (b_sl, inv_b'', b_cred))
  | (Buy, (Action (PickInventory inv_r'), _)) ->
      let inv_r'' = 
        if (List.fold_left2 find_cost 0 prices inv_r') <= cINITIAL_CASH then 
          inv_r'
        else def_inv in
      ((r_sl, inv_r'', r_cred), (b_sl, def_inv, b_cred))
  | (Buy, (_, Action (PickInventory inv_b'))) ->
      let inv_b'' =
        if (List.fold_left2 find_cost 0 prices inv_b') <= cINITIAL_CASH then
          inv_b'
        else def_inv in
      ((r_sl, def_inv, r_cred), (b_sl, inv_b'', b_cred))
  | (Buy, (_, _)) ->
      ((r_sl, def_inv, r_cred), (b_sl, def_inv, b_cred))
  | (BattleInit, (Action (SelectStarter r_start), 
                 Action (SelectStarter b_start))) -> 
      let r_start' = List.fold_left (fun a x -> if x.species = r_start then x 
                                           else a) (find_awake r_sl) r_sl in
      let b_start' = List.fold_left (fun a x -> if x.species = b_start then x
                                           else a) (find_awake b_sl) b_sl in
      let r_start'' = if r_start'.curr_hp = 0 then find_awake r_sl
                      else r_start' in
      add_update (SetChosenSteammon r_start''.species);
      let b_start'' = if b_start'.curr_hp = 0 then find_awake b_sl
                      else b_start' in
      add_update (SetChosenSteammon b_start''.species);
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (Action (SelectStarter r_start), _)) ->
      let r_start' = List.fold_left (fun a x -> if x.species = r_start then x 
                                           else a) (find_awake r_sl) r_sl in
      let r_start'' = if r_start'.curr_hp = 0 then find_awake r_sl
                      else r_start' in
      add_update (SetChosenSteammon r_start''.species);
      let b_start'' = find_awake b_sl in
      add_update (SetChosenSteammon b_start''.species);
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (_, Action (SelectStarter b_start))) ->
      let b_start' = List.fold_left (fun a x -> if x.species = b_start then x
                                           else a) (find_awake b_sl) b_sl in
      let r_start'' = find_awake r_sl in
      add_update (SetChosenSteammon r_start''.species);
      let b_start'' = if b_start'.curr_hp = 0 then find_awake b_sl
                      else b_start' in
      add_update (SetChosenSteammon b_start''.species);
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (_, _)) ->
      let r_start'' = find_awake r_sl in
      add_update (SetChosenSteammon r_start''.species);
      let b_start'' = find_awake b_sl in
      add_update (SetChosenSteammon b_start''.species);
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (Battle, (x, y)) -> do_battle curr_data x y
  | (Faint Red, (Action (SelectStarter r_start), _)) ->
      let r_start' = List.fold_left (fun a x -> if x.species = r_start then x 
                                           else a) (find_awake r_sl) r_sl in
      let r_start'' = if r_start'.curr_hp = 0 then find_awake r_sl
                      else r_start' in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      ((r_sl', r_inv, r_cred), bdata)
  | (Faint Red, (_, _)) ->
      let r_start'' = find_awake r_sl in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      ((r_sl', r_inv, r_cred), bdata)
  | (Faint Blue, (_, Action (SelectStarter b_start))) -> 
      let b_start' = List.fold_left (fun a x -> if x.species = b_start then x
                                           else a) (find_awake b_sl) b_sl in
      let b_start'' = if b_start'.curr_hp = 0 then find_awake b_sl
                      else b_start' in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      (rdata, (b_sl', b_inv, b_cred))
  | (Faint Blue, (_, _)) ->
      let b_start'' = find_awake b_sl in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      (rdata, (b_sl', b_inv, b_cred))
  | (Win Red, _)
  | (Win Blue, _)
  | (Tie, _) -> 
      failwith "State error in handle_step"

let handle_step g ra ba =
  let next_data = find_next_data g ra ba in
  State.update_state next_data;
  let (ca, cb) = match State.state_to_commands next_data with
                 | (DoNothing, DoNothing) -> (None, None)
                 | (DoNothing, y)         -> (None, Some y)
                 | (x, DoNothing)         -> (Some x, None)
                 | (x, y)                 -> (Some x, Some y) in
  let res = State.state_to_result () in
  (res, next_data, ca, cb)

let init_game () =
  let gs = (!(State.state), (([], [0;0;0;0;0;0;0], cSTEAMMON_CREDITS), 
                             ([], [0;0;0;0;0;0;0], cSTEAMMON_CREDITS))) in
  let r1 = TeamNameRequest in
  let r2 = TeamNameRequest in
  Initialization.init_pool "moves.csv" "steammon.csv";
  let al = hash_to_list Initialization.move_table in
  let sl = hash_to_list Initialization.mon_table in
  State.move_lst := al; State.mon_lst := sl;
  (gs, r1, r2, al, sl) 

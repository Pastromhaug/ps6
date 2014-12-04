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
  let first = begin match lst with 
    | [] -> failwith "Find cheapest list error"
    | h::_ -> h end in
  List.fold_left (fun a x -> if x.cost < a.cost then x else a) first lst

let find_awake (lst : steammon list) : steammon = 
  let first = begin match lst with
    | [] -> failwith "Find awake list error"
    | h::_ -> h end in
  List.fold_left (fun a x -> if x.curr_hp > a.curr_hp then x else a) first lst

type status_data = (steammon list * inventory) * (steammon list * inventory)

let initial_effects (sd : status_data) : status_data =
  let r_sl = fst (fst sd) in
  let b_sl = fst (snd sd) in
  begin match (r_sl, b_sl) with
  | ([], [])
  | ([], _)
  | (_, []) -> failwith "Steammon list error in initial_effects"
  | (rh::rt, bh::bt) -> 
  let mod_status (s : steammon) (col : color) : steammon = 
    let status' = begin match s.status with
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
                  | x -> x end in
    {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
     first_type = s.first_type; second_type = s.second_type;
     first_move = s.first_move; second_move = s.second_move;
     third_move = s.third_move; fourth_move = s.fourth_move;
     attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
     spl_defense = s.spl_defense; speed = s.speed; status = status';
     mods = s.mods; cost = s.cost} in
  let r_sl = (mod_status rh Red) :: rt in
  let b_sl = (mod_status bh Blue) :: bt in
  ((r_sl, snd(fst sd)),(b_sl, snd(snd sd))) end

let determine_order (sd : status_data) (r_act : command) (b_act : command) : (color * command) * (color * command) =
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

let dec_pp (sd : status_data) (mov : move) (col : color) : status_data =
        if mov.name = "Struggle" then sd else
        let data = match col with
                   | Red -> fst sd
                   | Blue -> snd sd in
        let (s,st) = match fst data with
                     | h::t -> (h,t)
                     | _ -> failwith "dec_pp list error" in
        let dec (mov : move) : move = 
          {name = mov.name; element = mov.element; target = mov.target; 
           max_pp = mov.max_pp; pp_remaining = mov.pp_remaining - 1; 
           power = mov.power; accuracy = mov.accuracy; effects= mov.effects} in
        let m_lst = [s.first_move; s.second_move; s.third_move; 
                                                  s.fourth_move] in
        let s = match m_lst with
        | x1::x2::x3::x4::[] -> let m1 = if x1.name = mov.name
                                         then dec x1
                                         else x1 in
                                let m2 = if x2.name = mov.name
                                         then dec x2
                                         else x2 in
                                let m3 = if x3.name = mov.name
                                         then dec x3
                                         else x3 in
                                let m4 = if x4.name = mov.name
                                         then dec x4
                                         else x4 in
          {species = s.species; curr_hp = s.curr_hp; max_hp = s.max_hp; 
           first_type = s.first_type; second_type = s.second_type;
           first_move = m1; second_move = m2; third_move = m3; 
           fourth_move = m4; attack = s.attack; spl_attack = s.spl_attack; 
           defense = s.defense; spl_defense = s.spl_defense; speed = s.speed; 
           status = s.status; mods = s.mods; cost = s.cost}
        | _ -> failwith "shouldn't be raised" in
        match col with
        | Red -> ((s::st, snd (fst sd)), snd sd)
        | Blue -> (fst sd, ((s::st), snd (snd sd)))

let find_target (mov : move) (sts : status option) : target =
  match (mov.target, sts) with
  | (User, _) -> User
  | (Opponent, Some Confused) -> if Random.int 99 < cSELF_ATTACK_CHANCE
                                 then User
                                 else Opponent
  | (Opponent, _) -> Opponent

let fold_effects (col : color) (targ : target) ((a, b) : status_data * (effect_result * color) list) (x : effect): status_data * (effect_result * color) list =
  let (att_l, def_l) = match (col, targ) with
                       | (Red, Opponent) -> (fst(fst a), fst(snd a))
                       | (Red, User) -> (fst(fst a), fst(fst a))
                       | (Blue, Opponent) -> (fst(snd a), fst(fst a))
                       | (Blue, User) -> (fst(snd a), fst(snd a)) in
  let (ah, at, dh, dt) = match (att_l, def_l) with
                         | ([], [])
                         | ([], _)
                         | (_, []) -> failwith "foldeffects list error"
                         | (ah::at, dh::dt) -> (ah, at, dh, dt) in
  match x with
  | InflictStatus sts -> begin match dh.status with
                               | Some _ -> (a, b)
                               | None -> 
    let any_asleep (lst : steammon list) : bool = 
      List.fold_left (fun a x -> a || x.status = Some Asleep) false lst
    in let any_frozen (lst : steammon list) : bool = 
      List.fold_left (fun a x -> a || x.status = Some Frozen) false lst
    in if (sts = Asleep && ah <> dh && any_asleep def_l) ||
          (sts = Frozen && any_frozen def_l) then (a, b)
       else let dh = 
    {species = dh.species; curr_hp = dh.curr_hp; max_hp = dh.max_hp; 
     first_type = dh.first_type; second_type = dh.second_type;
     first_move = dh.first_move; second_move = dh.second_move;
     third_move = dh.third_move; fourth_move = dh.fourth_move;
     attack = dh.attack; spl_attack = dh.spl_attack; 
     defense = dh.defense; spl_defense = dh.spl_defense; 
     speed = dh.speed; status = Some sts; mods = dh.mods; 
     cost = dh.cost} in 
    match (col, targ) with
    | (Red, Opponent)
    | (Blue, User) -> ((fst a, (dh::dt, snd (snd a))), 
                                ((InflictedStatus sts), Blue)::b)
    | (Blue, Opponent)
    | (Red, User) -> (((dh::dt, snd (fst a)), snd a), 
                                ((InflictedStatus sts), Red)::b) end
  | StatModifier (st, i) -> begin
      let mods'' =  dh.mods in
      let mods' = match st with
      | Atk ->  {attack_mod = mods''.attack_mod + i;
                 defense_mod = mods''.defense_mod;
                 spl_attack_mod = mods''.spl_attack_mod;
                 spl_defense_mod = mods''.spl_defense_mod;
                 speed_mod = mods''.speed_mod}
      | Def -> {attack_mod = mods''.attack_mod;
                defense_mod = mods''.defense_mod + i;
                spl_attack_mod = mods''.spl_attack_mod;
                spl_defense_mod = mods''.spl_defense_mod;
                speed_mod = mods''.speed_mod}
      | SpA -> {attack_mod = mods''.attack_mod;
                defense_mod = mods''.defense_mod;
                spl_attack_mod = mods''.spl_attack_mod + i;
                spl_defense_mod = mods''.spl_defense_mod;
                speed_mod = mods''.speed_mod}
      | SpD -> {attack_mod = mods''.attack_mod;
                defense_mod = mods''.defense_mod;
                spl_attack_mod = mods''.spl_attack_mod;
                spl_defense_mod = mods''.spl_defense_mod + i;
                speed_mod = mods''.speed_mod}
      | Spe -> {attack_mod = mods''.attack_mod;
                defense_mod = mods''.defense_mod;
                spl_attack_mod = mods''.spl_attack_mod;
                spl_defense_mod = mods''.spl_defense_mod;
                speed_mod = mods''.speed_mod + i} in
      let dh = 
       {species = dh.species; curr_hp = dh.curr_hp; max_hp = dh.max_hp; 
        first_type = dh.first_type; second_type = dh.second_type;
        first_move = dh.first_move; second_move = dh.second_move;
        third_move = dh.third_move; fourth_move = dh.fourth_move;
        attack = dh.attack; spl_attack = dh.spl_attack; 
        defense = dh.defense; spl_defense = dh.spl_defense; 
        speed = dh.speed; status = dh.status; mods = mods'; 
        cost = dh.cost} in
      match (col, targ) with
      | (Red, Opponent)
      | (Blue, User) -> ((fst a, (dh::dt, snd (snd a))), 
                                  ((StatModified (st, i)), Blue)::b)
      | (Blue, Opponent)
      | (Red, User) -> (((dh::dt, snd (fst a)), snd a), 
                                  ((StatModified (st, i)), Red)::b) end
  | RecoverPercent i -> begin
      let abs = (float_of_int i) *. 
                (float_of_int dh.max_hp) /.
                100. in
      let abs = int_of_float abs in
      let curr_hp' = min dh.max_hp (dh.curr_hp + abs) in
      let dh =
       {species = dh.species; curr_hp = curr_hp'; max_hp = dh.max_hp; 
        first_type = dh.first_type; second_type = dh.second_type;
        first_move = dh.first_move; second_move = dh.second_move;
        third_move = dh.third_move; fourth_move = dh.fourth_move;
        attack = dh.attack; spl_attack = dh.spl_attack; 
        defense = dh.defense; spl_defense = dh.spl_defense; 
        speed = dh.speed; status = dh.status; mods = dh.mods; 
        cost = dh.cost} in
      match (col, targ) with
      | (Red, Opponent)
      | (Blue, User) -> ((fst a, (dh::dt, snd (snd a))), 
                                  ((Recovered i), Blue)::b)
      | (Blue, Opponent)
      | (Red, User) -> (((dh::dt, snd (fst a)), snd a), 
                                  ((Recovered i), Red)::b) end
  | Recoil i -> begin
      let abs = (float_of_int i) *. 
                (float_of_int ah.max_hp) /.
                100. in
      let abs = int_of_float abs in
      let curr_hp' = max 0 (ah.curr_hp - abs) in
      let ah =
       {species = ah.species; curr_hp = curr_hp'; max_hp = ah.max_hp; 
        first_type = ah.first_type; second_type = ah.second_type;
        first_move = ah.first_move; second_move = ah.second_move;
        third_move = ah.third_move; fourth_move = ah.fourth_move;
        attack = ah.attack; spl_attack = ah.spl_attack; 
        defense = ah.defense; spl_defense = ah.spl_defense; 
        speed = ah.speed; status = ah.status; mods = ah.mods; 
        cost = ah.cost} in
      match col with
      | Red -> (((ah::at, snd (fst a)), snd a), 
                                  ((Recoiled i), Red)::b)
      | Blue -> ((fst a, (ah::at, snd (snd a))), 
                                  ((Recoiled i), Blue)::b) end
  | DamagePercent i -> begin
      let abs = (float_of_int i) *. 
                (float_of_int dh.max_hp) /.
                100. in
      let abs = int_of_float abs in
      let curr_hp' = max 0 (dh.curr_hp - abs) in
      let dh =
       {species = dh.species; curr_hp = curr_hp'; max_hp = dh.max_hp; 
        first_type = dh.first_type; second_type = dh.second_type;
        first_move = dh.first_move; second_move = dh.second_move;
        third_move = dh.third_move; fourth_move = dh.fourth_move;
        attack = dh.attack; spl_attack = dh.spl_attack; 
        defense = dh.defense; spl_defense = dh.spl_defense; 
        speed = dh.speed; status = dh.status; mods = dh.mods; 
        cost = dh.cost} in
      match (col, targ) with
      | (Red, Opponent)
      | (Blue, User) -> ((fst a, (dh::dt, snd (snd a))), 
                                  ((Damaged i), Blue)::b)
      | (Blue, Opponent)
      | (Red, User) -> (((dh::dt, snd (fst a)), snd a), 
                                  ((Damaged i), Red)::b) end
  | HealStatus stslst -> begin 
      let sts' = match dh.status with
                 | Some sts -> if List.mem sts stslst then None
                               else Some sts
                 | None -> None in
      let dh =
       {species = dh.species; curr_hp = dh.curr_hp; max_hp = dh.max_hp; 
        first_type = dh.first_type; second_type = dh.second_type;
        first_move = dh.first_move; second_move = dh.second_move;
        third_move = dh.third_move; fourth_move = dh.fourth_move;
        attack = dh.attack; spl_attack = dh.spl_attack; 
        defense = dh.defense; spl_defense = dh.spl_defense; 
        speed = dh.speed; status = sts'; mods = dh.mods; 
        cost = dh.cost} in
      match (col, targ) with
      | (Red, Opponent)
      | (Blue, User) -> begin match sts' with 
                        | Some sts -> ((fst a, (dh::dt, snd (snd a))), 
                                       ((HealedStatus sts), Blue)::b)
                        | None -> ((fst a, (dh::dt, snd (snd a))), b)
                        end
                                  
      | (Blue, Opponent)
      | (Red, User) -> begin match sts' with 
                       | Some sts -> (((dh::dt, snd (fst a)), snd a), 
                                       ((HealedStatus sts), Red)::b) 
                       | None -> (((dh::dt, snd (fst a)), snd a), b) 
                       end
      end
  | RestorePP i -> begin
  let add_pp (m : move) : move = 
  {name = m.name; element = m.element; target = m.target;
   max_pp = m.max_pp; pp_remaining = min m.max_pp (m.pp_remaining + i);
   power = m.power; accuracy = m.accuracy; effects = m.effects} in
  let first_move' = add_pp dh.first_move in
  let second_move' = add_pp dh.second_move in
  let third_move' = add_pp dh.third_move in
  let fourth_move' = add_pp dh.fourth_move in
  let dh = 
  {species = dh.species; curr_hp = dh.curr_hp; max_hp = dh.max_hp; 
   first_type = dh.first_type; second_type = dh.second_type;
   first_move = first_move'; second_move = second_move';
   third_move = third_move'; fourth_move = fourth_move';
   attack = dh.attack; spl_attack = dh.spl_attack; 
   defense = dh.defense; spl_defense = dh.spl_defense; speed= dh.speed; 
   status = dh.status; mods = dh.mods; cost = dh.cost} in
   match (col, targ) with
   | (Red, Opponent)
   | (Blue, User) -> ((fst a, (dh::dt, snd (snd a))), 
                               ((RestoredPP i), Blue)::b)
   | (Blue, Opponent)
   | (Red, User) -> (((dh::dt, snd (fst a)), snd a), 
                               ((RestoredPP i), Red)::b) end

let move_occurs (mov : move) (targ : target) : bool =
        match targ with
        | User -> true
        | Opponent -> Random.int 99 < mov.accuracy

let dmg_ref = ref 0
let efft_ref = ref Ineffective

let do_move (sd : status_data) (mov : move) (col : color) (targ : target) : status_data = 
        let (r_sl, b_sl) = (fst (fst sd), fst (snd sd)) in
        let (rh, rt, bh, bt) = match (r_sl, b_sl) with
                               | ([], [])
                               | ([], _)
                               | (_, []) -> failwith "do_move list error"
                               | (rh::rt, bh::bt) -> (rh, rt, bh, bt) in
        let (attacker, defender) = match (col, targ) with
                                   | (Red, Opponent) -> (rh, bh)
                                   | (Blue, Opponent) -> (bh, rh) 
                                   | (Red, User) -> (rh, rh)
                                   | (Blue, User) -> (bh, bh) in
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
        let dmg = calculate_damage atk def pow mult in
        dmg_ref := min defender.curr_hp dmg;
        efft_ref := efft;
        let def' = {species = defender.species; curr_hp = max (defender.curr_hp - dmg) 0; 
                    max_hp = defender.max_hp; first_type = defender.first_type; 
                    second_type = defender.second_type; first_move = defender.first_move; 
                    second_move = defender.second_move; third_move = defender.third_move; 
                    fourth_move = defender.fourth_move; attack = defender.attack; 
                    spl_attack = defender.spl_attack; defense = defender.spl_defense; 
                    spl_defense = defender.spl_defense; speed = defender.spl_defense; 
                    status = defender.status; mods = defender.mods; cost = defender.cost} in
        let sd = match (col, targ) with
                 | (Red, Opponent)
                 | (Blue, User) -> (fst sd, (def'::bt, snd (snd sd)))
                 | (Red, User) 
                 | (Blue, Opponent) ->  ((def'::rt, snd (fst sd)), snd sd) in
        sd

let do_action (sd : status_data) (col : color) (act : command) : status_data =
  let sl = match col with
           | Red -> fst (fst sd)
           | Blue -> fst (snd sd) in
  match act with
  | Action (SwitchSteammon str) -> begin
      let s = List.fold_left (fun a x -> if x.species = str then x 
                                         else a) (find_awake sl) sl in
      let sl' = s :: (List.filter ((<>) s) sl) in
      add_update (SetChosenSteammon s.species);
      match col with
      | Red -> ((sl', snd (fst sd)), snd sd)
      | Blue -> (fst sd, (sl', snd (snd sd))) end
  | Action (UseItem (itm, str)) -> begin
      let item_pp (s : steammon) : steammon =
        let add_pp (m : move) : move = 
          {name = m.name; element = m.element; target = m.target;
           max_pp = m.max_pp; pp_remaining = min m.max_pp (m.pp_remaining + 5);
           power = m.power; accuracy = m.accuracy; effects = m.effects} in
        let first_move' = add_pp s.first_move in
        let second_move' = add_pp s.second_move in
        let third_move' = add_pp s.third_move in
        let fourth_move' = add_pp s.fourth_move in
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
      let (sl, inv) = match col with
                      | Red -> fst sd
                      | Blue -> snd sd in
      let ok_to_use (str : string) (lst : steammon list) : bool = 
        List.fold_left (fun a x -> a || (x.species = str && x.curr_hp > 0)) 
                                                                false lst in
      let exists (str : string) (lst : steammon list) : bool =
        List.fold_left (fun a x -> a || x.species = str) false lst in
      let has_status (str: string) (lst : steammon list) : bool =
        List.fold_left (fun a x -> a || (x.species = str && x.status <> None))
                                                                false lst in
      let find_status (str : string) (lst : steammon list) : status option =
        List.fold_left (fun a x -> match a with
                                   | Some y -> Some y
                                   | None -> if x.species = str then x.status
                                             else None) None lst in
        
      let data = match itm with
      | Ether -> begin match inv with
                       | x::t when x > 0 && 
                                   exists str sl &&  
                                   ok_to_use str sl -> 
                           let inv' = (x-1)::t in
                           let sl' = List.map (fun x -> if x.species = str 
                                                          then item_pp x
                                                        else x) sl in 
                           add_update (Item("Ether",(RestoredPP 5), col, str));
                           (sl', inv')
                       | x::t when x > 0 && 
                                   exists str sl && 
                                   not (ok_to_use str sl) ->
                           let inv' = (x-1)::t in
                           (sl, inv')
                       | _ -> (sl, inv) end
      | MaxPotion -> begin match inv with
                           | h::x::t when x > 0 &&
                                          exists str sl &&
                                          ok_to_use str sl ->
                             let inv' = h::(x-1)::t in
                             let sl' = List.map (fun x -> 
                                                  if x.species = str
                                                    then item_hp x x.max_hp
                                                  else x) sl in
                             let s = Table.find Initialization.mon_table str in
                             add_update (Item("MaxPotion",(Recovered s.max_hp),
                                           col, str));
                             (sl', inv')
                           | h::x::t when x > 0 &&
                                          exists str sl &&
                                          not (ok_to_use str sl) ->
                             let inv' = h::(x-1)::t in
                             (sl, inv')
                           | _ -> (sl, inv) end
      | Revive -> begin match inv with
                        | h1::h2::x::t when x > 0 &&
                                            exists str sl &&
                                            not (ok_to_use str sl) -> 
                             let inv' = h1::h2::(x-1)::t in
                             let sl' = List.map (fun x -> 
                                         if x.species = str
                                         then item_status 
                                           (item_hp x (x.max_hp/2))
                                         else x) sl in
                             let s = Table.find Initialization.mon_table str in
                             add_update (Item("Revive",(Recovered(s.max_hp/2)),
                                           col, str));
                             (sl', inv')
                        | h1::h2::x::t when x > 0 &&
                                            exists str sl &&
                                            ok_to_use str sl ->
                              let inv' = h1::h2::(x-1)::t in
                              (sl, inv')
                        | _ -> (sl, inv) end
      | FullHeal -> begin match inv with
                          | h1::h2::h3::x::t when x > 0 &&
                                                  exists str sl &&
                                                  ok_to_use str sl &&
                                                  has_status str sl ->
                            let sts = match find_status str sl with
                                      | None -> failwith "Error with FullHeal"
                                      | Some sts' -> sts' in
                            let inv' = h1::h2::h3::(x-1)::t in
                            let sl' = List.map (fun x ->
                                        if x.species = str
                                        then item_status x
                                        else x) sl in
                            add_update (Item("FullHeal",(HealedStatus sts),
                                          col, str));
                            (sl', inv')
                          | h1::h2::h3::x::t when x > 0 &&
                                                  exists str sl ->
                             let inv' = h1::h2::h3::(x-1)::t in
                             (sl, inv')
                          | _ -> (sl, inv) end
      | XAttack -> begin match inv with
                         | h1::h2::h3::h4::x::t when x > 0 &&
                                                 exists str sl &&
                                                 ok_to_use str sl ->
                           let inv' = h1::h2::h3::h4::(x-1)::t in
                           let sl' = List.map (fun x ->
                                       if x.species = str
                                       then item_mods x Atk
                                       else x) sl in
                           add_update (Item("XAttack",StatModified (Atk, 1),
                                         col, str));
                           (sl', inv')
                         | h1::h2::h3::h4::x::t when x > 0 &&
                                                 exists str sl ->
                           let inv' = h1::h2::h3::h4::(x-1)::t in
                           (sl, inv')
                         | _ -> (sl, inv) end
      | XDefense -> begin match inv with
                          | h1::h2::h3::h4::h5::x::t when x > 0 &&
                                                      exists str sl &&
                                                      ok_to_use str sl ->
                            let inv' = h1::h2::h3::h4::h5::(x-1)::t in
                            let sl' = List.map (fun x ->
                                        if x.species = str
                                        then item_mods x Def
                                        else x) sl in
                            add_update (Item("XDefense",StatModified (Def, 1),
                                          col, str));
                            (sl', inv')
                          | h1::h2::h3::h4::h5::x::t when x > 0 &&
                                                      exists str sl ->
                            let inv' = h1::h2::h3::h4::h5::(x-1)::t in
                           (sl, inv')
                          | _ -> (sl, inv) end
      | XSpeed -> begin match inv with
                        | h1::h2::h3::h4::h5::h6::x::[] when x > 0 &&
                                                    exists str sl &&
                                                    ok_to_use str sl ->
                          let inv' = h1::h2::h3::h4::h5::h6::(x-1)::[] in
                          let sl' = List.map (fun x ->
                                      if x.species = str
                                      then item_mods x Spe
                                      else x) sl in
                          add_update (Item("XSpeed",StatModified (Spe, 1),
                                        col, str));
                          (sl', inv')
                        | h1::h2::h3::h4::h5::h6::x::[] when x > 0 &&
                                                    exists str sl ->
                          let inv' = h1::h2::h3::h4::h5::h6::(x-1)::[] in
                          (sl, inv')
                        | _ -> (sl, inv) end in      
      match col with
      | Red -> (data, snd sd)
      | Blue -> (fst sd, data)
    end
  | Action (UseMove str) -> begin
      let s = match sl with
              | h::_ -> h
              | _ -> failwith "list error in UseMove" in
      if s.curr_hp = 0 then sd else
      match get_move_from_steammon s str with
      | None -> sd
      | Some mov ->
      match s.status with
      | Some Asleep -> add_update (Move {name = str; element = mov.element;
                                         from = col; toward = invert_color col; 
                                         damage = 0; hit = Failed Asleep; 
                                         effectiveness = Ineffective;
                                         effects = []}); 
                       sd 
      | Some Frozen -> add_update (Move {name = str; element = mov.element;
                                         from = col; toward = invert_color col;
                                         damage = 0; hit = Failed Frozen; 
                                         effectiveness = Ineffective;
                                         effects = []}); 
                       sd
      | Some Paralyzed when Random.int 99 < cPARALYSIS_CHANCE -> 
          add_update (Move {name = str; element = mov.element; from = col;
                            toward = invert_color col; damage = 0;
                            hit = Failed Paralyzed; 
                            effectiveness = Ineffective; effects = []}); 
          sd
      | _ -> 
      let mov = if mov.pp_remaining = 0 then 
                  Table.find Initialization.move_table "Struggle"
                else mov in
      let targ = find_target mov s.status in
      let eff_lst = match mov.effects with
                      | None -> []
                      | Some (lst, _, chance) ->
                          if Random.int 99 < chance then lst
                          else [] in
      let (sd, ercl) = List.fold_left(fold_effects col targ) (sd,[]) eff_lst in
      let def_col = match (col, targ) with
                    | (Red, Opponent)
                    | (Blue, User) -> Blue
                    | (Red, User)
                    | (Blue, Opponent) -> Red in
      let sd = if move_occurs mov targ 
               then (add_update 
                      (Move {name = mov.name; element = mov.element; from = col;
                             toward = def_col; damage = !dmg_ref; hit = Hit; 
                             effectiveness = !efft_ref; effects = ercl}); 
                     do_move sd mov col targ)
               else (add_update 
                      (Move {name = mov.name; element = mov.element;
                             from = col; toward = def_col; damage = 0;
                             hit = Miss; effectiveness = Ineffective;
                             effects = []});
                    sd) in
      (dec_pp sd mov col) end
  | _ -> sd

let after_effects (sd : status_data) : status_data =
  let r_sl = fst (fst sd) in
  let b_sl = fst (snd sd) in
  match (r_sl, b_sl) with
  | ([], [])
  | ([], _)
  | (_, []) -> failwith "Steammon list error in after_effects"
  | (rh::rt, bh::bt) -> 
  let mod_hp (s : steammon) (col : color) : steammon = 
    let max_hp = float_of_int s.max_hp in
    let curr_hp'= match s.status with
                  | Some Poisoned -> let dmg = min s.curr_hp 
                                     (int_of_float (max_hp *. cPOISON_DAMAGE))
                                     in add_update (AdditionalEffects
                                               [(DamagedByStatus 
                                                (dmg, Poisoned), col)]); 
                                     s.curr_hp - dmg
                  | Some Burned -> let dmg = min s.curr_hp
                                   (int_of_float (max_hp *. cBURN_DAMAGE))
                                   in add_update (AdditionalEffects
                                             [(DamagedByStatus
                                              (dmg, Burned), col)]);
                                   s.curr_hp - dmg
                  | _ -> s.curr_hp in
    {species = s.species; curr_hp = curr_hp'; max_hp = s.max_hp; 
     first_type = s.first_type; second_type = s.second_type;
     first_move = s.first_move; second_move = s.second_move;
     third_move = s.third_move; fourth_move = s.fourth_move;
     attack = s.attack; spl_attack = s.spl_attack; defense = s.defense;
     spl_defense = s.spl_defense; speed = s.speed; status = s.status;
     mods = s.mods; cost = s.cost} in
  let r_sl = (mod_hp rh Red) :: rt in
  let b_sl = (mod_hp bh Blue) :: bt in
  ((r_sl, snd(fst sd)),(b_sl, snd(snd sd))) 

let do_battle (gsd : game_status_data) (r_act : command) (b_act : command) : game_status_data =
  let ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred)) = gsd in
  let sd = ((r_sl, r_inv), (b_sl, b_inv)) in
  let sd = initial_effects sd in
  let ((first, act1), (second, act2)) = determine_order sd r_act b_act in
  let sd = do_action sd first act1 in
  let sd = do_action sd second act2 in
  let sd = after_effects sd in
  let ((r_sl, r_inv), (b_sl, b_inv)) = sd in
  ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred))

let find_next_data (g : game) (ra : command) (ba : command) : game_status_data =
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

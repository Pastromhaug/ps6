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

let find_next_data (g : game) (ra : command) (ba : command) : game_output =
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
      curr_data
  | (Init, (_, Action (SendTeamName strb))) ->
      curr_data
  | (Init, (Action (SendTeamName stra), _)) ->
      curr_data
  | (Init, (_, _)) -> 
      curr_data
  | (Draft Red, (Action (PickSteammon stra), _)) -> 
      let sa = Table.find Initialization.mon_table stra in
      let s = if List.mem sa !(State.mon_lst) && r_cred - sa.cost >= 0 then sa
              else find_cheapest !(State.mon_lst) in
      State.mon_lst := List.filter ((<>) s) State.mon_lst;
      if r_cred - s.cost >= 0 then ((s::r_sl, r_inv, r_cred - s.cost), bdata)
      else ((s::r_sl, r_inv, 0), bdata)
  | (Draft Red, (_, _)) -> 
      let s = find_cheapest !(State.mon_lst) in
      if r_cred - s.cost >= 0 then ((s::r_sl, r_inv, r_cred - s.cost), bdata)
      else ((s::r_sl, r_inv, 0), bdata)
  | (Draft Blue, (_, Action (PickSteammon strb))) ->
      let sb = Table.find Initialization.mon_table strb in
      let s = if List.mem sb !(State.mon_lst) && b_cred - sb.cost >= 0 then sb
              else find_cheapest !(State.mon_lst) in
      State.mon_lst := List.filter ((<>) s) State.mon_lst;
      if b_cred - s.cost >= 0 then (rdata, (s::b_sl, b_inv, b_cred - s.cost))
      else (rdata, (s::b_sl, b_inv, 0))
  | (Draft Blue, (_, _)) ->
      let s = find_cheapest !(State.mon_lst) in
      if b_cred - s.cost >= 0 then (r_data, (s::b_sl, b_inv, b_cred - s.cost))
      else (rdata, (s::b_sl, b_inv, 0))
  | (Buy, (Action (PickInventory inv_r'), Action (PickInventory inv_b'))) ->
      let inv_r'' = 
        if (List.fold_left2 find_cost 0 prices inv_x) <= cINITIAL_CASH then 
          inv_r'
        else def_inv in
      let inv_b'' =
        if (List.fold_left2 find_cost 0 prices inv_y) <= cINITIAL_CASH then
          inv_b'
        else def_inv in
      ((r_sl, inv_r'', r_cred), (b_sl, inv_b'', b_cred))
  | (Buy, (Action (PickInventory inv_r'), _)) ->
      let inv_r'' = 
        if (List.fold_left2 find_cost 0 prices inv_x) <= cINITIAL_CASH then 
          inv_r'
        else def_inv in
      ((r_sl, inv_r'', r_cred), (b_sl, def_inv, b_cred))
  | (Buy, (_, Action (PickInventory inv_b'))) ->
      let inv_b'' =
        if (List.fold_left2 find_cost 0 prices inv_y) <= cINITIAL_CASH then
          inv_b'
        else def_inv in
      ((r_sl, def_inv, r_cred), (b_sl, inv_b'', b_cred))
  | (Buy, (_, _)) ->
      ((r_sl, def_inv, r_cred), (b_sl, def_inv, b_cred))
  | (BattleInit, (Action (SelectStarter r_start), 
                 Action (SelectStarter b_start))) -> 
      let r_start' = Table.find Initialization.mon_table r_start in
      let b_start' = Table.find Initialization.mon_table b_start in
      let r_start'' = if r_start'.hp = 0 then find_awake r_sl
                      else r_start' in
      let b_start'' = if b_start'.hp = 0 then find_awake b_sl
                      else b_start' in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (Action (SelectStarter r_start), _)) ->
      let r_start' = Table.find Initialization.mon_table r_start in
      let r_start'' = if r_start'.hp = 0 then find_awake r_sl
                      else r_start' in
      let b_start'' = find_awake b_sl in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (_, Action (SelectStarter b_start))) ->
      let b_start' = Table.find Initialization.mon_table b_start in
      let r_start'' = find_awake r_sl in
      let b_start'' = if b_start'.hp = 0 then find_awake b_sl
                      else b_start' in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (BattleInit, (_, _)) ->
      let r_start'' = find_awake r_sl in
      let b_start'' = find_awake b_sl in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      ((r_sl', r_inv, r_cred), (b_sl', b_inv, b_cred))
  | (Battle, _) -> failwith "BECAUSE PER MADE ME DO THIS"
  | (Faint Red, (Action (SelectStarter r_start), _)) ->
      let r_start' = Table.find Initialization.mon_table r_start in
      let r_start'' = if r_start'.hp = 0 then find_awake r_sl
                      else r_start' in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      ((r_sl', r_inv, r_cred), bdata)
  | (Faint Red, (DoNothing, _)) ->
      let r_start'' = find_awake r_sl in
      let r_sl' = r_start'' :: (List.filter ((<>) r_start'') r_sl) in
      ((r_sl', r_inv, r_cred), bdata)
  | (Faint Blue, (_, Action (SelectStarter b_start))) -> 
      let b_start' = Table.find Initialization.mon_table b_start in
      let b_start'' = if b_start'.hp = 0 then find_awake b_sl
                      else b_start' in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      (rdata, (b_sl', b_inv, b_cred))
  | (Faint Blue, (_, DoNothing)) ->
      let b_start'' = find_awake b_sl in
      let b_sl' = b_start'' :: (List.filter ((<>) b_start'') b_sl) in
      (rdata, (b_sl', b_inv, b_cred))
  | (Win Red, _)
  | (Win Blue, _)
  | (Tie, _) -> 
      failwith "State error in handle_step"

let handle_step g ra ba = failwith "DURR"

let init_game () =
  let gs = (!(State.state), (([], [0;0;0;0;0;0;0], cNUM_CREDITS), 
                             ([], [0;0;0;0;0;0;0], cNUM_CREDITS))) in
  let r1 = TeamNameRequest in
  let r2 = TeamNameRequest in
  Initialization.init_pool moves.csv steammon.csv;
  let al = hash_to_list Initialization.move_table in
  let sl = hash_to_list Initialization.mon_table in
  State.move_lst := al; State.mon_lst := sl;
  (gs, r1, r2, al, sl) 

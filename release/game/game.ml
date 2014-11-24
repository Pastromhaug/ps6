open Definitions
open Util
open Constants
open Netgraphics

(* You have to implement this. Change it from int to yout own state type*)
type game = State.t * game_status_data

let game_datafication g = snd g
	
let game_from_data game_data = (!(State.state), game_data)

let find_next_data g ra ba =
  let (ra', ba') = match (ra, ba) with
  | (Action a, Action b) -> (a, b)
  | (Action a, _) -> (a, a)
  | (Action _, b) -> (b, b) 
  | (_, _) -> failwith "Command error from players in handle_step" in
  let curr_data = snd g in
  let (rdata, bdata) = curr_data
  let ((r_sl, r_inv, r_cred), (b_sl, b_inv, b_cred)) = (rdata, bdata) in
  match (fst g, (ra', ba')) with
  | (Init, (SendTeamName stra, SendTeamName strb)) -> 
      curr_data
  | (Draft Red, (PickSteammon stra, _)) -> 
      let sa = Table.find Initialization.mon_table stra in
      if List.mem sa !(State.mon_lst) then ((sa::r_sl, r_inv, r_cred), bdata)
      else failwith "GIVE IT THE CHEAPEST STEAMMON"
  | (Draft Blue, (_, PickSteammon strb)) ->
      let sb = Table.find Initialization.mon_table strb in
      if List.mem sb !(State.mon_lst) then (rdata, (sb::b_sl, b_inv, b_cred))
      else failwith "ALSO GIVE IT THE CHEAPEST STEAMMON"
  | (Buy, )
  | (BattleInit, )
  | (Battle Red, )
  | (Battle Blue, )
  | (Faint Red, )
  | (Faint Blue, )
  | (Win Red, )
  | (Win Blue, )
  | (Tie, ) -> 
      failwith "State error in handle_step"
  | (_, _) -> 
      failwith "Command error from players in handle_step2"

let handle_step g ra ba =

let init_game () =
  let gs = (!(State.state), (([], [0;0;0;0;0;0;0], cNUM_CREDITS), 
                             ([], [0;0;0;0;0;0;0], cNUM_CREDITS)) in
  let r1 = TeamNameRequest in
  let r2 = TeamNameRequest in
  Initialization.init_pool moves.csv steammon.csv;
  let al = hash_to_list Initialization.move_table in
  let sl = hash_to_list Initialization.mon_table in
  State.move_lst := al; State.mon_lst := sl;
  (gs, r1, r2, al, sl)
  

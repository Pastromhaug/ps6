(* State.ml *)
(* see state.mli for documentation *)
open Definitions

type t = 
    Init 
  | Draft of color 
  | Buy 
  | BattleInit
  | Battle
  | Faint of color
  | Win of color
  | Tie

let state = ref Init
let mon_lst = ref []
let move_lst = ref []
let red_picks = ref Constants.cNUM_PICKS
let blue_picks = ref Constants.cNUM_PICKS

let update_state next_data =
  let extract_hp (x : steammon) : int = x.curr_hp in
  let fold_help (a : bool) (x : int) : bool = a || (x > 0) in
  match (!state, next_data) with
  | (Init, _) -> 
      if (Random.int 99) < 50 then state := (Draft Red)
      else state := (Draft Blue)
  | (Draft Red, _) ->
      red_picks := !red_picks - 1;
      if !red_picks = 0 && !blue_picks = 0 then state := Buy
      else if !red_picks >= !blue_picks then ()
      else state := (Draft Blue)
  | (Draft Blue, _) ->
      blue_picks := !blue_picks -1;
      if !red_picks = 0 && !blue_picks = 0 then state := Buy
      else if !blue_picks >= !red_picks then ()
      else state := (Draft Red)
  | (Buy, _) ->
      state := BattleInit
  | (BattleInit, _) ->
      state := Battle
  | (Battle, ((rsl, _, _), (bsl, _, _))) -> begin
      let (rhp, bhp) = (List.map extract_hp rsl, List.map extract_hp bsl) in
      let r_has_steammon = List.fold_left fold_help false rhp in
      let b_has_steammon = List.fold_left fold_help false bhp in
      match (rhp, bhp, r_has_steammon, b_has_steammon) with
      | ([], [], _, _)
      | ([], _, _, _)
      | (_, [], _, _) -> failwith "Steammon list error"
      | (0::_, 0::_, true, true) -> state := BattleInit
      | (0::_, 0::_, false, false) -> state := Tie
      | (_::_, 0::_, true, false) -> state := (Win Red)
      | (0::_, _::_, false, true) -> state := (Win Blue)
      | (0::_, _::_, true, true) -> state := (Faint Red)
      | (_::_, 0::_, true, true) -> state := (Faint Blue)
      | (_::_, _::_, true, true) -> () 
      | (_::_, _::_, _, _) -> failwith "Steammon list error" end
  | (Faint Blue, _)
  | (Faint Red, _) -> state := Battle
  | (Win Red, _)
  | (Win Blue, _)
  | (Tie, _) -> ()

let state_to_commands next_data =
  match !state with
  | Init -> 
      failwith "State error"
  | Draft Red -> 
      (Request(PickRequest(Red,next_data,!(move_lst),!(mon_lst))), 
       DoNothing)
  | Draft Blue -> 
      (DoNothing, 
       Request(PickRequest(Blue,next_data,!(move_lst),!(mon_lst))))
  | Buy -> 
      (Request (PickInventoryRequest next_data), 
       Request (PickInventoryRequest next_data))
  | BattleInit ->
      (Request (StarterRequest next_data), 
       Request (StarterRequest next_data))
  | Battle ->
      (Request (ActionRequest next_data),
       Request (ActionRequest next_data))
  | Faint Red ->
      (Request (StarterRequest next_data),
       DoNothing)
  | Faint Blue ->
      (DoNothing,
       Request (StarterRequest next_data))
  | Win Red
  | Win Blue
  | Tie ->
      (DoNothing,
       DoNothing)

let state_to_result () =
  match !state with
  | Init
  | Draft Red
  | Draft Blue
  | Buy
  | BattleInit
  | Battle
  | Faint Red
  | Faint Blue -> None
  | Win Red -> Some (Winner Red)
  | Win Blue -> Some (Winner Blue)
  | Tie -> Some Tie

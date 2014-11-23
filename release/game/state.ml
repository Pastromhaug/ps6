(* State.ml *)
(* see state.mli for documentation *)
(* open Definitions *)

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
let update_state next_data =
  let extract_hp (x : steammon) : int = x.curr_hp in
  let fold_help (a : bool) (x : int) : bool = a || (x > 0) in
  match (!state, next_data) with
  | (Init, _) -> 
      if (Random.int 99) < 50 then state := (Draft Red)
      else state := (Draft Blue)
  | (Draft _, ((_, _, 0), (_, _, 0))) ->
      state := Buy
  | (Draft Red, ((_, _, red_cred), (_, _, blue_cred))) ->
      if red_cred >= blue_cred then ()
      else state := (Draft Blue)
  | (Draft Blue, ((_, _, red_cred), (_, _, blue_cred))) ->
      if blue_cred >= red_cred then ()
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

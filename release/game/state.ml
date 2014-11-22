(* State.ml *)
(* see state.mli for documentation *)
open Definitions

type t = Init | Draft of color | Buy | Battle of color

let state = ref Init
let update_state gsd =
  match (!state, gsd) with
  | (Init, _) -> 
      if (Random.int 99) < 50 then state := (Draft Red)
      else state := (Draft Blue
  | (Draft _, ((_, _, 0), (_, _, 0))) ->
      state := Buy
  | (Draft Red, ((_, _, red_cred), (_, _, blue_cred))) ->
      if red_cred >= blue_cred then state := (Draft Red)
      else state := (Draft Blue)
  | (Draft Blue, ((_, _, red_cred), (_, _, blue_cred))) ->
      if blue_cred >= red_cred then state := (Draft Blue)
      else state := (Draft Red)
  | _ -> failwith "TODO"

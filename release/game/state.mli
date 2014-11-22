(* State.mli *)
open Definitions

(* type of state *)
type t

(* pointer to a state *)
val state : t ref

(* updates state based on game_status_data and current state
 * requires : current game_status_data *)
val update_state : game_status_data -> unit

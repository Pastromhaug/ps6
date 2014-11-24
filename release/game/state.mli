(* State.mli *)
open Definitions

(* type of state *)
type t

(* pointer to a state *)
val state : t ref

(* pointer to a list of moves *)
val move_lst : 'a list ref

(* pointer to a list of steammon *)
val mon_lst : 'a list ref

(* updates state based on game_status_data and current state
 * requires : current game_status_data *)
val update_state : game_status_data -> unit

(* determines the next commands to send based on the next state and the next
 * game status data
 * returns : pair of commands (red, blue) to send *)
val state_to_commands : game_status_data -> command * command

(* determines game_result option based on the next state 
 * returns : game_result *)
val state_to_result : unit -> game_result option

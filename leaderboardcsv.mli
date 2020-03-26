(**Functions to update frame when game state is leaderboard. *)

open Gamesetup

(** [tuple_lst] is a list of the top 10  high scoring user's information. *)
val tuple_lst : (string * int * string) list ref

(** [score_info] is the abstract type for representing the user's scoring 
    information, in the form (user_name * score * date). *)
type score_info = string * int * string

exception FormatError

(** [write_to_csv fname score_info_lst] writes the top 10 scorer's info 
    [score_info_lst] to file [fname]. *)
val write_to_csv : string -> unit

(** [read_from_csv fname] is reads the top 10 saved scorer's information from 
    file [fname] then saves it by changing the reference [tuple_lst]. *)
val read_from_csv : string -> unit

(** [game_over_score_info state] is the current's user's scoring information. *)
val game_over_score_info : Gamesetup.state -> score_info

(** [add_cur_score_info] updates the score info list to include the current
    player's score info. *)
val add_cur_score_info : score_info -> unit

(** [update_top_score_info] updates the top 10 scorer's score info. *)
val update_top_score_info : unit -> unit

(** [get_tuple_lst] is the list of score info. *)
val get_tuple_lst : unit -> score_info list

(** [renew_leaderboard] is the updated game state for when the game state is
    Leaderboard. *)
val renew_leaderboard : state -> state

(** [draw_leaderboard] draws the current game state when the game state is 
    Leaderboard. *)
val draw_leaderboard : state -> state

(** [parse_exit_leaderboard ()] is true if the exit button is clicked while the
    current state is leaderboard. *)
val parse_exit_leaderboard : unit -> bool
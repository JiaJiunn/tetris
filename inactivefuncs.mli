(**Functions to update frame when game state is inactive. *)

open Graphics 
open Gamesetup
open Drawfuncs

(** [draw_inactive_screen ()] draws current inactive screen. *)
val draw_inactive_screen : unit -> unit

(** [parse_start_inactive ()] returns true if mouse clicking start button *) 
val parse_start_inactive : unit -> bool

(** [parse_tutorial_inactive ()] returns true if mouse clicking tutorial 
button *) 
val parse_tutorial_inactive : unit -> bool

(** [parse_leaderboard_inactive ()] returns true if mouse clicking leaderboard
 button *) 
val parse_leaderboard_inactive : unit -> bool

(** [parse_settings_inactive ()] returns true if mouse clicking settings
 button *) 
val parse_settings_inactive : unit -> bool

(** [same_button ()] returns true if the mouse is hovering on the same button 
    as last frame *) 
val same_button : unit -> bool
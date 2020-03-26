(**Functions to update frame when game state is pause. *)
open Gamesetup 
open Graphics 
open Drawfuncs

(** [draw_pause_screen ()] draws the display in the pause screen. *)
val draw_pause_screen : unit -> unit

(** [parse_resume_pause ()] is true if the user selects the resume button in
 the pause screen. *)
val parse_resume_pause : unit -> bool

(** [parse_exit_pause ()] is true if the user selects the exit button in the
    pause screen. *)
val parse_exit_pause : unit -> bool
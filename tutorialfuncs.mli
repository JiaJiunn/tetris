(**Functions to update frame when game state is tutorial. *)

open Gamesetup

(** [draw_tutorial_screen ()] draws the tutorial screen to the game display. *)
val draw_tutorial_screen : unit -> unit

(** [parse_exit_tutorial ()] is true when the exit button is pressed while the
    current state is tutorial. *)
val parse_exit_tutorial : unit -> bool
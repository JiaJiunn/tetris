(** Functions to accumulate block, remove filled lines, add scores *)

open Gamesetup

(** [laser_bomb state] is the game state [state] with a row removed. *)
val laser_bomb : state -> state 

(** [score_Lines state] returns [state] if cur_block hits bottom (edge or 
    other block under). Otherwise, returns [state] with lines removed and score 
    added, and a new block generated*)
val score_lines : state -> state

(** [game_over_collides screen block] is true if a newly generated block 
[block] fulfills the game over conditions in [screen]. *)
val game_over_collides : screen -> block -> bool
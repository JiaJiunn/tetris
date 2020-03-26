(**Functions related to drawing tetro game in graphics GUI. *)
open Gamesetup 
open Graphics
open Movetetro

(** [square_size] is the size of a square block. *)
val square_size : int

(** [str_to_list str] is the characters of the string [str] in a list. *)
val str_to_list : string -> string list

(** [draw_word (x0, y0) s lst] draws the words in list [lst]. *)
val draw_word : (int * int) -> int -> Graphics.color -> string list -> unit

(** [draw_pixel_frame (x, y)] draws a pixel's frame at coordinate [x, y] with*)
val draw_pixel_frame : (int * int) -> unit

(** [draw_pixel_filled (x, y)] draws a pixel at coordinate Graphics coordinate 
[x, y] with color filled in*)
val draw_pixel_filled : (int * int) -> unit

(** [draw_block block] draws the block [block]. *)
val draw_block : block -> unit

(** [draw_ghost_piece block] draws the "ghost piece" of the given [block], ie 
the position on the screen the piece would land after a hard drop command. *)
val draw_ghost_piece : block -> unit

(** [draw_queue block_lst] draws the 3 [blocks] that are next up in the queue 
of the game off to the side of the game screen, in order. *)
val draw_queue : block list -> unit

(** [draw_held_piece block] draws the given [block] which is currently held 
in the game. *)
val draw_held_piece : block option -> unit

(** [draw_score num] writes the current [score] value of the player off to the
 side of the game screen. *)
val draw_score : int -> unit 

(** [draw_screen screen] draws the screen [screen]. *)
val draw_screen : screen -> unit 

(** [draw_borders state] draws all the borders of the boxes in the game 
window *)
val draw_borders : state -> unit

(************************ ROUND BOIS ************************)

(** [draw_block block] draws the round block onto the game screen. *)
val draw_block_round : block -> unit

(** [draw_ghost_piece block] draws the "ghost piece" of the given [block], the 
    position on the screen the piece would land after a hard drop command. *)
val draw_ghost_piece_round : block -> unit

(** [draw_queue block_lst] draws the 3 [blocks] that are next up in the queue 
of the game off to the side of the game screen, in order. *)
val draw_queue_round : block list -> unit

(** [draw_held_piece block] draws the given [block] which is currently held in
 the game. *)
val draw_held_piece_round : block option -> unit

(** [draw_screen_round screen] draws the screen [screen] when the current block
    shape used is round. *)
val draw_screen_round : screen -> unit 

(** [draw_active_screen_rec state] draws current active screen with rectangle 
    block. *)  
val draw_active_screen_rec : state -> unit

(** [draw_active_screen_round state] draws current active screen with round 
    block *) 
val draw_active_screen_round : state -> unit
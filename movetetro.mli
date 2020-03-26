(** Functions related to the move of tetro block. *)  
open Gamesetup

(** [hit_bottom state] returns true if any bottom surface of the falling block 
    has collided with another fallen block or the bottom of the screen. *)
val hit_bottom : state -> bool

(** [move_block_left state] returns a new state with [cur_block] moved
    one unit to the left if the block can be moved to that position without 
    colliding with anything. Otherwise, return the orignal state. *)
val move_block_left : state -> state

(** [move_block_right state] returns a new state with [cur_block] moved
    one unit to the right if the block can be moved to that position without 
    colliding with anything. Otherwise, return the orignal state. *)
val move_block_right : state -> state

(** [move_block_down_soft state] returns a new state with [block] moved
    one unit down if the block can be moved to that position without 
    colliding with anything. Otherwise, return the orignal state. *)
val move_block_down_soft : state -> state 

(** [move_block_down_hard state] returns a new state with [block] moved
    as far down as it can go before colliding with another block or the bottom
    of the screen. *)
val move_tetro_down_hard : state -> state 

(** [rotate_cw state] returns a new state with [cur_block] rotated 90 degrees
    clockwise if the block can be rotated to that position without 
    colliding with anything. Otherwise, return the orignal state. *)
val rotate_cw : state -> state

(** [rotate_ccw state] returns a new state with [cur_block] rotated 90 degrees
    counterclockwise if the block can be rotated to that position without 
    colliding with anything. Otherwise, return the orignal state. *)
val rotate_ccw : state -> state
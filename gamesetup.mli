(** Functions related to game setup/config and utility functions. *)
open Graphics
open Unix

(** [difficulty] represents the different levels of difficulty. *)
type difficulty = Easy | Medium | Hard 

(** [colors] represents the different colors available. *)
type colors = Cyan | Blue | Orange | Yellow | Green | Red | Purple | Black 
            | White | Freeze

(** [palettes] are the different color palettes available. *)
type palettes = Classic | Grayscale | NightMode | Sunset | Bamboo
              | Flamingo | Caramel | Pastel | Beach | Watermelon

(** [game_st] represents the different game states. *)
type game_st = Inactive | Active | Pause | Settings | Leaderboard | Tutorial 

(** [block] is the abstract type representing blocks in the game. *)
type block = {
  pos : int * int;
  pixels : (int * int) list;
  color : colors
}

(** [screen] is the abstract type representing the game screen. *)
type screen = colors array array 

(** [state] is the abstract type representing the game state. *)
type state = {
  cur_block : block;
  queue : block list;
  screen : screen;
  score : int;
  score_multiple : int;
  num_lines_removed : int;
  num_lines_multiple : int;
  refresh: bool;
  timer : float;
  held_block : block option;
  can_swap : bool;
  hit_timer : float;
  hit_counter: int;
  game_state : game_st;
  user_name : string;
}

exception WrongDimensions

val user_name : string ref
val screen_width : int ref
val screen_height : int ref
val drop_time : float ref
val base_time : float ref
val is_rect : bool ref
val start_pos_x : int ref
val start_pos_y : int ref
val min_screen_width : int
val min_screen_height : int
val max_screen_width : int
val max_screen_height : int
val temp_difficulty : difficulty ref 
val difficulty : difficulty ref 
val color_palette : (colors -> Graphics.color) ref
val pixel_size  : int
val buffer : int
val piece_border : int
val sidebar_pix : int
val sidebar_w : int
val sidebar_h : int
val holdbar_h : int
val line_points : int
val num_lines_removed_once  : int
val add_points_for_multi_lines  : int
val points_for_speed_up  : int
val speed_up_time  : float
val min_droptime : float
val num_lines_removed_for_powerup : int
val time_moving_after_hit : float
val button_w : int
val button_h : int

(** [parse_variables ()] opens a "I/O" window, which asks the user to customize
    their game experience. This function also takes those inputs and set the 
    config. *) 
val parse_variables : unit -> unit

(** [new_I_block (start_pos_x, start_pos_y)] is a new I block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_I_block : int * int -> block

(** [new_J_block (start_pos_x, start_pos_y)] is a new J block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_J_block : int * int -> block

(** [new_L_block (start_pos_x, start_pos_y)] is a new L block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_L_block : int * int -> block

(** [new_O_block (start_pos_x, start_pos_y)] is a new O block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_O_block : int * int -> block

(** [new_S_block (start_pos_x, start_pos_y)] is a new S block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_S_block : int * int -> block

(** [new_Z_block (start_pos_x, start_pos_y)] is a new Z block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_Z_block : int * int -> block

(** [new_T_block (start_pos_x, start_pos_y)] is a new T block spawned at 
position [(start_pos_x, start_pos_y)]. *)
val new_T_block : int * int -> block

(** [new_bomb_block (start_pos_x, start_pos_y)] is a new bomb block spawned 
at position [(start_pos_x, start_pos_y)]. *)
val new_bomb_block : int * int -> block

(** [new_freeze_block (start_pos_x, start_pos_y)] is a new freeze block 
spawned at position [(start_pos_x, start_pos_y)]. *)
val new_freeze_block : int * int -> block

(** [new_block ()] is a new random block with equal probability of having a I, 
J, L, O, S, Z or T shape. *)
val new_block : unit -> block

(** [new_power_up ()] is a new random block with equal probability of being a 
    bomb or freeze block. *)
val new_power_up : state -> bool -> block

(** [block_to_screen_pos (x, y)] returns a (x', y') that converts block pos 
    [(x, y)] to screen coordinate (x', y'). The difference is that 
    Block pos or Graphics pos: (0, 0) is at lower left. 
    Screen pos or Array matrix: (0, 0) is at upper left. 
    Also Array.make_matrix x y returns a rect with height x and width y. 
    Not the other way around*)
val block_to_screen_pos : int * int -> int * int

(** [block_pos block] returns a list of tuple that represents the coordinate 
    of each pixel contained by this block at its current position *) 
val block_pixels : block -> (int * int) list

(** [parse_button x_min x_max y_min y_max] is true if the user clicks the mouse
    while hovering inside the box with sides by [x_min x_max y_min y_max]. *)
val parse_button : int -> int -> int -> int -> bool

(** [parse_button_hover x_min x_max y_min y_max] is true if the user's mouse
    hovers over the the box with sides given by [x_min x_max y_min y_max]. *)
val parse_button_hover : int -> int -> int -> int -> bool

(** [freeze drop_time multiple] takes in a float ref [drop_time] and an integer 
    [multiple] and modifies the contents of the ref to !drop_time * multiple *)
val freeze : float ref -> int -> unit

(** [defreeze drop_time divisor] takes in a float ref [drop_time] and an 
    integer [divisor] and modifies the contents of the ref to 
    !drop_time / multiple *)
val defreeze : float ref -> int -> unit

(** [swap_with_held_block state] is the game state [state] with the current 
block swapped with the held block, if there is a held block. Otherwise, 
the current block will be held, while the the current block will be the next 
block on the queue. Note that this does not check if the block is allowed to
 be swapped. *)
val swap_with_held_block : state -> state

(** [classic colors] matches the colors used [colors] to their respective rgb 
    values according to the [classic] template. *)
val classic : colors -> Graphics.color

(** [grayscale colors] matches the colors used [colors] to their respective rgb 
    values according to the [grayscale] template. *)
val grayscale : colors -> Graphics.color

(** [night_mode colors] matches the colors used [colors] to their respective 
rgb values according to the [night_mode] template. *)
val night_mode : colors -> Graphics.color

(** [sunset colors] matches the colors used [colors] to their respective rgb 
    values according to the [sunset] template. *)
val sunset : colors -> Graphics.color

(** [bamboo colors] matches the colors used [colors] to their respective rgb 
    values according to the [bamboo] template. *)
val bamboo : colors -> Graphics.color

(** [flamingo colors] matches the colors used [colors] to their respective rgb 
    values according to the [flamingo] template. *)
val flamingo : colors -> Graphics.color

(** [caramel colors] matches the colors used [colors] to their respective rgb 
    values according to the [caramel] template. *)
val caramel : colors -> Graphics.color

(** [pastel colors] matches the colors used [colors] to their respective rgb 
    values according to the [pastel] template. *)
val pastel : colors -> Graphics.color

(** [beach colors] matches the colors used [colors] to their respective rgb 
    values according to the [beach] template. *)
val beach : colors -> Graphics.color

(** [watermelon colors] matches the colors used [colors] to their respective 
rgb values according to the [watermelon] template. *)
val watermelon : colors -> Graphics.color

(** [pick_palette p] takes in a palette and returns a function that will 
    that draws the corresponding colors. *)
val pick_palette : palettes -> colors -> Graphics.color
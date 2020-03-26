(**Functions to update frame when game state is settings. *)

open Gamesetup 
open Graphics 

(** [config] is the configuration parameters for the game. *)
type config = {
  screen_height : int ref;
  screen_width : int ref; 
  drop_time : float ref;
  base_time : float ref;
  is_rect : bool ref;
  start_pos_x : int ref;
  start_pos_y : int ref; 
  color_palette : (colors ->Graphics.color) ref
}

(** [main_config] stores the settings which build the game. *)
val main_config : config 

(** [temp_config] stores the settings picked by the user which may be 
    discarded if the user cancels their changes. If the user saves, 
    [temp_config] is used to update [main_config]. *)
val temp_config : config 

(** [make_change (x,y) state] updates the [temp_config] when a button other 
    than Cancel or Save Changes is clicked. If the user saves, [main_config] is 
    reassigned to the settings in [temp_config], and the game_state becomes 
    Inactive. If the user cancels, [temp_config] is reverted to what it 
    was originally, and the game state becomes Inactive. *)
val make_change : (int * int) -> state -> state

(** [draw_settings_screen state] calls on all the drawing functions to make
    the settings screen appear. *)
val draw_settings_screen : state -> unit 
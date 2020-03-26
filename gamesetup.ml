open Graphics
open Unix

(*************** Config ****************)

let user_name = ref "Anonymous"
let screen_width = ref 10
let screen_height = ref 20
let drop_time = ref 2.5
let base_time = ref 2.5 

let is_rect = ref true

let start_pos_x = ref 5
let start_pos_y = ref 20

let min_screen_width = 5
let min_screen_height = 17 
let max_screen_width = 40
let max_screen_height = 40

type difficulty = | Easy | Medium | Hard 

let temp_difficulty = ref Easy 
let difficulty = ref Easy 

exception WrongDimensions

(******************* Color Palattes ********************) 
type colors = Cyan | Blue | Orange | Yellow | Green | Red | Purple | Black 
            | White | Freeze

type palettes = Classic | Grayscale | NightMode | Sunset | Bamboo
              | Flamingo | Caramel | Pastel | Beach | Watermelon

let classic color = 
  match color with 
  | Cyan -> Graphics.rgb 126 252 252
  | Blue -> Graphics.rgb 31 1 255
  | Orange -> Graphics.rgb 237 151 32
  | Yellow -> Graphics.rgb 253 255 0
  | Green -> Graphics.rgb 122 252 22
  | Red -> Graphics.rgb 230 46 45 
  | Purple -> Graphics.rgb 152 25 255
  | Black -> Graphics.black
  | White -> Graphics.white
  | Freeze -> Graphics.rgb 178 211 238 

let grayscale color = 
  match color with 
  | Cyan -> Graphics.rgb 120 120 120
  | Blue -> Graphics.rgb 55 55 55
  | Orange -> Graphics.rgb 150 150 150
  | Yellow -> Graphics.rgb 100 100 100
  | Green -> Graphics.rgb 40 40 40
  | Red -> Graphics.rgb 135 135 135
  | Purple -> Graphics.rgb 70 70 70
  | Black -> Graphics.black
  | White -> Graphics.white
  | Freeze -> Graphics.rgb 170 170 170

let night_mode color = 
  match color with 
  | Cyan -> Graphics.rgb 180 215 250
  | Blue -> Graphics.rgb 100 105 200
  | Orange -> Graphics.rgb 230 170 100
  | Yellow -> Graphics.rgb 220 140 120
  | Green -> Graphics.rgb 120 210 150
  | Red -> Graphics.rgb 180 60 110
  | Purple -> Graphics.rgb 150 100 190
  | Black -> Graphics.black
  | White -> Graphics.rgb 30 30 90
  | Freeze -> Graphics.rgb 226 252 255 

let sunset color = 
  match color with 
  | Cyan -> Graphics.rgb 173 162 204
  | Blue -> Graphics.rgb 177 45 74
  | Orange -> Graphics.rgb 228 128 53
  | Yellow -> Graphics.rgb 226 156 82
  | Green -> Graphics.rgb 120 136 188
  | Red -> Graphics.rgb 214 112 142
  | Purple -> Graphics.rgb 150 108 166
  | Black -> Graphics.black
  | White -> Graphics.rgb 249 212 204
  | Freeze -> Graphics.rgb 178 211 238 

let bamboo color = 
  match color with 
  | Cyan -> Graphics.rgb 150 209 82
  | Blue -> Graphics.rgb 126 85 16
  | Orange -> Graphics.rgb 96 138 98
  | Yellow -> Graphics.rgb 179 217 52
  | Green -> Graphics.rgb 129 188 57 
  | Red -> Graphics.rgb 164 124 43
  | Purple -> Graphics.rgb 88 156 144
  | Black -> Graphics.black
  | White -> Graphics.rgb 217 232 187
  | Freeze -> Graphics.rgb 178 211 238 

let flamingo color = 
  match color with 
  | Cyan -> Graphics.rgb 236 185 201
  | Blue -> Graphics.rgb 44 54 100
  | Orange -> Graphics.rgb 196 102 144
  | Yellow -> Graphics.rgb 232 154 131
  | Green -> Graphics.rgb 66 91 148 
  | Red -> Graphics.rgb 218 126 147
  | Purple -> Graphics.rgb 203 73 95
  | Black -> Graphics.black
  | White -> Graphics.rgb 243 220 219
  | Freeze -> Graphics.rgb 178 211 238 

let caramel color = 
  match color with 
  | Cyan -> Graphics.rgb 101 166 132
  | Blue -> Graphics.rgb 161 116 76
  | Orange -> Graphics.rgb 229 196 132
  | Yellow -> Graphics.rgb 139 68 14
  | Green -> Graphics.rgb 194 129 30 
  | Red -> Graphics.rgb 61 34 18
  | Purple -> Graphics.rgb 118 88 45
  | Black -> Graphics.black
  | White -> Graphics.rgb 236 223 199
  | Freeze -> Graphics.rgb 178 211 238  

let pastel color = 
  match color with 
  | Cyan -> Graphics.rgb 220 253 245
  | Blue -> Graphics.rgb 209 220 254
  | Orange -> Graphics.rgb 245 220 201
  | Yellow -> Graphics.rgb 250 243 206
  | Green -> Graphics.rgb 218 251 204
  | Red -> Graphics.rgb 239 203 203
  | Purple -> Graphics.rgb 221 203 251
  | Black -> Graphics.rgb 136 133 168 
  | White -> Graphics.rgb 155 210 230 
  | Freeze -> Graphics.rgb 226 252 255 

let beach color = 
  match color with 
  | Cyan -> Graphics.rgb 159 209 216
  | Blue -> Graphics.rgb 111 166 184
  | Orange -> Graphics.rgb 187 172 154
  | Yellow -> Graphics.rgb 173 201 229
  | Green -> Graphics.rgb 211 190 158
  | Red -> Graphics.rgb 118 149 193
  | Purple -> Graphics.rgb 169 229 221
  | Black -> Graphics.black 
  | White -> Graphics.rgb 237 234 239
  | Freeze -> Graphics.rgb 178 211 238  

let watermelon color = 
  match color with 
  | Cyan -> Graphics.rgb 208 74 58
  | Blue -> Graphics.rgb 228 137 120
  | Orange -> Graphics.rgb 97 165 80
  | Yellow -> Graphics.rgb 85 124 90
  | Green -> Graphics.rgb 222 78 69
  | Red -> Graphics.rgb 200 39 57
  | Purple -> Graphics.rgb 228 129 123
  | Black -> Graphics.black 
  | White -> Graphics.rgb 243 207 191 
  | Freeze -> Graphics.rgb 215 239 232  

let pick_palette (p : palettes) : (colors -> Graphics.color) = 
  match p with 
  | Classic -> classic 
  | Grayscale -> grayscale
  | NightMode -> night_mode
  | Sunset -> sunset
  | Bamboo -> bamboo
  | Flamingo -> flamingo 
  | Caramel -> caramel
  | Pastel -> pastel 
  | Beach -> beach 
  | Watermelon -> watermelon

let color_palette = ref classic

(*************** Parse Input ****************)
(** [parse_name ()] opens an I/O window for username. *)  
let rec parse_name () = 
  print_endline "Please tell us your name.";
  print_string "> ";
  let arg = read_line () in 
  if String.length arg > 15 
  then (print_endline "That name is too long; please try again!\n"; 
        parse_name ())
  else arg

let parse_variables () =
  print_endline "Welcome to Terminal Tetris!\n";
  user_name := parse_name ();
  ()

(*************** CONSTANTS ****************)

let pixel_size = 20
let buffer = 5
let piece_border = 3
let sidebar_pix = pixel_size / 2
let sidebar_w = 5 
let sidebar_h = 10 
let holdbar_h = 4  
let line_points = 10
let num_lines_removed_once = 3
let add_points_for_multi_lines = 5
let points_for_speed_up = 5*line_points
let speed_up_time = 0.4 
let min_droptime = 0.1 
let num_lines_removed_for_powerup = 5
let time_moving_after_hit = 0.7
let button_w = 4
let button_h = 1

(*************** Data Structures ****************)
type game_st = Inactive | Active | Pause | Settings | Leaderboard | Tutorial 

type block = {
  pos : int * int;
  pixels : (int * int) list;
  color : colors
}

type screen = colors array array 

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

(******************* BLOCKS ********************) 

let new_I_block (start_pos_x, start_pos_y) = {
  pos = (start_pos_x, start_pos_y - 1);
  pixels = [(-1, 0); 
            (0, 0);
            (1, 0);
            (2, 0)]; 
  color = Cyan
}
let new_J_block (start_pos_x, start_pos_y)= {
  pos = (start_pos_x, start_pos_y - 2);
  pixels = [(-1, 1); 
            (-1, 0);
            (0, 0);
            (1, 0)]; 
  color = Blue
}
let new_L_block (start_pos_x, start_pos_y) = {
  pos = (start_pos_x, start_pos_y - 2);
  pixels = [(1, 1); 
            (-1, 0);
            (0, 0);
            (1, 0)]; 
  color = Orange
}
let new_O_block (start_pos_x, start_pos_y)= {
  pos = (start_pos_x, start_pos_y - 1);
  pixels = [(0, 0); 
            (1, 0);
            (0, -1);
            (1, -1)]; 
  color = Yellow
}
let new_S_block (start_pos_x, start_pos_y)= {
  pos = (start_pos_x, start_pos_y - 1);
  pixels = [(0, 0); 
            (1, 0);
            (-1, -1);
            (0, -1)]; 
  color = Green
}
let new_Z_block (start_pos_x, start_pos_y)= {
  pos = (start_pos_x, start_pos_y - 1);
  pixels = [(0, 0); 
            (-1, 0);
            (1, -1);
            (0, -1)]; 
  color = Red
}
let new_T_block (start_pos_x, start_pos_y) = {
  pos = (start_pos_x, start_pos_y - 2);
  pixels = [(0, 0); 
            (0, 1);
            (-1, 0);
            (1, 0)]; 
  color = Purple
}

let new_bomb_block (start_pos_x, start_pos_y) = {
  pos = (start_pos_x, start_pos_y -1);
  pixels = [(0,0)];
  color = Black
}
let new_freeze_block (start_pos_x, start_pos_y) = {
  pos = (start_pos_x, start_pos_y -1);
  pixels = [(0,0)];
  color = Freeze
}

let new_block () =
  let block_id = Random.int 7 in
  match block_id with 
  | 0 -> new_I_block (!start_pos_x, !start_pos_y)
  | 1 -> new_J_block (!start_pos_x, !start_pos_y)
  | 2 -> new_L_block (!start_pos_x, !start_pos_y)
  | 3 -> new_O_block (!start_pos_x, !start_pos_y)
  | 4 -> new_S_block (!start_pos_x, !start_pos_y)
  | 5 -> new_Z_block (!start_pos_x, !start_pos_y)
  | 6 -> new_T_block (!start_pos_x, !start_pos_y)
  | x -> failwith "This line is unreachable"

let new_power_up state can_power_up = 
  if can_power_up then
    let power_up_id = Random.int 2 in 
    match power_up_id with 
    | 0 -> new_bomb_block (!start_pos_x, !start_pos_y)
    | 1 -> new_freeze_block (!start_pos_x, !start_pos_y)
    | x -> failwith "This line is unreachable."
  else new_block ()

(************** Utility Functions  *****************)

let block_to_screen_pos (x, y) = 
  (!screen_height - 1 - y, x)

let block_pixels (block:block) = 
  let (x0, y0) = block.pos in 
  List.map (function (x, y) -> (x+x0, y+y0)) block.pixels

let parse_button x_min x_max y_min y_max = 
  let mouse_event = Graphics.wait_next_event [ Graphics.Poll ] in
  let mouse_xcoord = mouse_event.Graphics.mouse_x in
  let mouse_ycoord = mouse_event.Graphics.mouse_y in
  button_down () && (mouse_xcoord > x_min) && (mouse_xcoord < x_max) && 
  (mouse_ycoord > y_min) && (mouse_ycoord < y_max)

let parse_button_hover x_min x_max y_min y_max = 
  let mouse_event = Graphics.wait_next_event [ Graphics.Poll ] in
  let mouse_xcoord = mouse_event.Graphics.mouse_x in
  let mouse_ycoord = mouse_event.Graphics.mouse_y in
  (mouse_xcoord > x_min) && (mouse_xcoord < x_max) && 
  (mouse_ycoord > y_min) && (mouse_ycoord < y_max)

(***************** Freeze BLOCK ******************) 

let freeze d m = d := (!d *. (float_of_int m))

let defreeze d m = 
  let x = (!d /. (float_of_int m)) in 
  d := max x !base_time

(***************** HELD BLOCK ******************) 

(** [hold_helper_new_cur state] is the game state [state] with the current 
block taken off the queue, and the queue updated with a new block. *)
let hold_helper_new_cur state =
  let new_bl = List.hd state.queue in 
  let new_queue = (List.tl state.queue) @ [new_block ()] in
  {state with cur_block = new_bl; queue = new_queue; can_swap = true}

(** [add_block_to_queue] is the game state with the current block added to a
    queue, and a new block generated. *)
let add_block_to_hold state =
  let to_hold = state.cur_block in
  let new_block_stt = hold_helper_new_cur state in
  match to_hold.color with
  | Cyan ->
    {new_block_stt with 
     held_block = Some (new_I_block (!start_pos_x, !start_pos_y)); 
     can_swap = false}
  | Blue ->
    {new_block_stt with 
     held_block = Some (new_J_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Orange ->
    {new_block_stt with 
     held_block = Some (new_L_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Yellow ->
    {new_block_stt with 
     held_block = Some (new_O_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Green ->
    {new_block_stt with 
     held_block = Some (new_S_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Red ->
    {new_block_stt with 
     held_block = Some (new_Z_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Purple ->
    {new_block_stt with 
     held_block = Some (new_T_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Black ->
    {new_block_stt with 
     held_block = Some (new_bomb_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | Freeze ->
    {new_block_stt with 
     held_block = Some (new_freeze_block (!start_pos_x, !start_pos_y));
     can_swap = false}
  | White -> failwith "Shouldn't swap white blocks"

let swap_with_held_block state =
  let prev_cur_block = state.cur_block in
  let block_color = prev_cur_block.color in
  match state.held_block with
  | None -> add_block_to_hold state
  | Some block -> 
    match block_color with
    | Cyan ->
      {state with 
       cur_block = block; 
       held_block = Some (new_I_block (!start_pos_x, !start_pos_y)); 
       can_swap = false}
    | Blue ->
      {state with 
       cur_block = block;
       held_block = Some (new_J_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Orange ->
      {state with 
       cur_block = block;
       held_block = Some (new_L_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Yellow ->
      {state with 
       cur_block = block;
       held_block = Some (new_O_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Green ->
      {state with 
       cur_block = block;
       held_block = Some (new_S_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Red ->
      {state with 
       cur_block = block;
       held_block = Some (new_Z_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Purple ->
      {state with 
       cur_block = block;
       held_block = Some (new_T_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Black ->
      {state with 
       cur_block = block;
       held_block = Some (new_bomb_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | Freeze -> 
      {state with 
       cur_block = block;
       held_block = Some (new_freeze_block (!start_pos_x, !start_pos_y));
       can_swap = false}
    | White -> failwith "shouldn't swap white blocks"

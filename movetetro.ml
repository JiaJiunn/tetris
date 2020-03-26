open Gamesetup

(************************ COLLISION DETECTION ************************)

let hit_bottom state = 
  let block = state.cur_block in 
  let screen = state.screen in 
  List.exists (function (x, y) ->
      let (x', y') = block_to_screen_pos (x, y)  in
      y = 0 || screen.(x'+1).(y') <> White )
    (block_pixels block)

(** [block_edge_collides block] returns true if [block]  "runs out of"  
    screen edge. *)
let block_edge_collides block = 
  List.exists (fun (x, y) -> 
      x < 0 || x > !screen_width - 1 || y > !screen_height - 1 || y < 0) 
    (block_pixels block)

(** [block_block_collides screen block] return true if [block] collides with 
    another block (i.e. existing non white color in [screen]). 
    Note: should never run into Index out of bound error because we only call 
    it if this block is not on the edge*) 
let block_block_collides screen block = 
  List.exists (fun (x, y) -> 
      let (x', y') = block_to_screen_pos (x, y) in 
      screen.(x').(y') <> White
    ) (block_pixels block)

(** [block_collides state] returns true if [block] collides with anything *) 
let block_collides block screen = 
  block_edge_collides block || block_block_collides screen block

(************************ BLOCK MOVEMENT ************************)

(** [move_x block x] moves [block] horizontally by [x] units. *)
let move_x (block : block) x = 
  {block with pos = (fst block.pos + x, snd block.pos)} 

(** [move_y block y] moves [block] vertically by [y] units. *)
let move_y (block: block) y = 
  {block with pos = (fst block.pos, snd block.pos + y)}

let move_block_left state =
  let screen = state.screen in
  let moved_block = move_x state.cur_block (-1) in
  if not (block_collides moved_block screen) then 
    {state with cur_block = moved_block}
  else 
    state

let move_block_right state =
  let screen = state.screen in
  let moved_block = move_x state.cur_block 1 in
  if not (block_collides moved_block screen) then 
    {state with cur_block = moved_block}
  else 
    state

let move_block_down_soft state = 
  let screen = state.screen in 
  let moved_block = move_y state.cur_block (-1) in 
  if not (block_collides moved_block screen) then
    {state with cur_block = moved_block}
  else state

let move_tetro_down_hard state = 
  let rec hard_drop_helper state = 
    if (hit_bottom state) then state 
    else let moved_block = move_y state.cur_block (-1) in 
      (hard_drop_helper {state with cur_block = moved_block}) in
  hard_drop_helper state 

(************************ ROTATE BLOCK ************************)

(**[rotate_cw_helper] transforms the (x, y) coordinates of the [pixels] of a 
   block so to rotate it 90 degrees clockwise. *)
let rec rotate_cw_helper (lst : (int * int) list) =
  match lst with 
  | [] -> []
  | (x, y) :: t -> (y, -x) :: (rotate_cw_helper t)

let rotate_cw state  = 
  let screen = state.screen in 
  let block = state.cur_block in
  let rotated_block = 
    {block with pixels = (rotate_cw_helper block.pixels)} in
  if not (block_collides rotated_block screen) && not(block.color = Yellow) 
  then {state with cur_block = rotated_block} 
  else state

(**[rotate_ccw_helper] transforms the (x, y) coordinates of the [pixels] of a 
   block so to rotate it 90 degrees counterclockwise. *)
let rec rotate_ccw_helper (lst : (int * int) list) =
  match lst with 
  | [] -> []
  | (x, y) :: t -> (-y, x) :: (rotate_ccw_helper t)

let rotate_ccw state = 
  let screen = state.screen in 
  let block = state.cur_block in
  let rotated_block = 
    {block with pixels = (rotate_ccw_helper block.pixels)} in
  if not (block_collides rotated_block screen) && not(block.color = Yellow) 
  then {state with cur_block = rotated_block} 
  else state


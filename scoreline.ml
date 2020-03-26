open Gamesetup
open Movetetro
open Unix

(** [accumulate_block state] draws the current block onto the game screen. *)
let accumulate_block state = 
  let block = state.cur_block in 
  let screen = state.screen in 
  let block_pix = block_pixels block in 
  List.iter (fun (x, y) -> 
      let (x', y') = block_to_screen_pos (x, y) in
      (screen.(x').(y') <- block.color)) block_pix;
  state

(** [range i j] is an int list [i, i+1, ... j-1] *) 
let rec range i j = if i >= j then [] else i :: (range (i+1) j)

(** [filled_row x screen] returns true if all pixels with in row x 
    (in screen_pos) have non-White colors in [screen]. *) 
let filled_row x screen = 
  let y_lst = range 0 !screen_width in
  List.for_all (fun y -> screen.(x).(y) <> White) y_lst

(** [filled_lines screen] is the list of filled lines on the current game 
display. *)
let filled_lines screen = 
  let x_lst = range 0 !screen_height in 
  List.filter (fun x -> filled_row x screen) x_lst

(** [color_shift x screen] changes screen by replacing colors at row [x] 
    by colors at row [x-1]. If [x] = 0, then use WHITE. *)
let color_shift x screen = 
  for y = 0 to (!screen_width - 1) do 
    if x = 0 
    then screen.(x).(y) <- White 
    else screen.(x).(y) <- screen.(x - 1).(y)
  done

let laser_bomb s = 
  let sblock = s.cur_block in 
  let spos = block_to_screen_pos sblock.pos in 
  let sposx = fst spos in
  for x_idx = sposx downto 0 do 
    color_shift x_idx s.screen
  done;
  {s with num_lines_removed = s.num_lines_removed + 1 ; 
          score = s.score + 1 * line_points; refresh = true}

(** [remove_lines state] returns [state'] with updated [score] [screen] 
    [num_lines_removed]. Update means removing filled_lines on current 
    [screen]*)
let remove_lines state = 
  if state.cur_block.color = Black then 
    if hit_bottom state then (laser_bomb state), false
    else {state with refresh = true}, false
  else 
    let filled_xs = filled_lines state.screen in 
    let new_score = if List.length filled_xs < num_lines_removed_once then
        (state.score + (List.length filled_xs) * line_points) else 
        (state.score + (List.length filled_xs) * line_points + 
         add_points_for_multi_lines)  in
    let new_score_multiple = new_score / points_for_speed_up in
    let new_num_lines = state.num_lines_removed + List.length filled_xs in
    let new_num_lines_multiple = new_num_lines / num_lines_removed_for_powerup 
    in 
    List.iter (function x -> 
        for x_idx = x downto 0 do 
          color_shift x_idx state.screen
        done
      ) filled_xs; 
    (if new_score_multiple > state.score_multiple 
     then
       (let potential_new_speed =  (!drop_time -. speed_up_time) in 
        if potential_new_speed < min_droptime then ()
        else (drop_time := potential_new_speed; 
              base_time := potential_new_speed;))
     else ());
    if new_num_lines_multiple > state.num_lines_multiple
    then 
      {state with score = new_score; refresh = true; 
                  score_multiple = new_score_multiple;
                  num_lines_removed = new_num_lines; 
                  num_lines_multiple = new_num_lines_multiple}, true
    else 
      {state with score = new_score; refresh = true; 
                  score_multiple = new_score_multiple;
                  num_lines_removed = new_num_lines; 
                  num_lines_multiple = new_num_lines_multiple}, false

let game_over_collides screen block = 
  List.exists (fun (x, y) -> 
      let (x', y') = block_to_screen_pos (x, y) in 
      screen.(x' + 1).(y') <> White || screen.(x').(y') <> White
    )
    (block_pixels block)

(** [new_cur_block state b] is [state] with randomly generated new block if
    possible. If [b] is true, the next generated block will be a power-up. 
    If it is not possible to spawn any block, the game ends. *)
let new_cur_block (state, (can_power_up : bool))=
  let new_bl = List.hd state.queue in 
  let new_queue = (List.tl state.queue) @ [new_power_up state can_power_up] in
  if (game_over_collides state.screen new_bl) then 
    {state with game_state = Leaderboard} 
  else
    {state with cur_block = new_bl; queue = new_queue; can_swap = true}

let score_lines state = 
  state |> accumulate_block |> remove_lines |> new_cur_block


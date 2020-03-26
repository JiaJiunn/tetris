open Graphics
open Unix
open Gamesetup
open Movetetro
open Scoreline
open Drawfuncs
open Inactivefuncs
open Pausefuncs
open Settingsfuncs
open Leaderboardcsv
open Tutorialfuncs

(*********** Init ************)  

(** [init_window] sets up the display window for the entire program. *)
let init_window () =
  Random.self_init (); 
  open_graph " ";
  set_window_title "Terminal Tetris";
  resize_window (!screen_width*pixel_size + buffer*3 + sidebar_w*pixel_size) 
    (!screen_height*pixel_size + buffer *2) ;
  ()

(** [screen_init color] is an abstract representation of the game screen, 
    with all pixels initialized to [color]. *)
let screen_init color = Array.make_matrix !screen_height !screen_width color

(** [init_state] is the initial game state when the game first starts. *)
let init_state () = 
  {
    cur_block = new_block ();
    queue = [new_block (); new_block (); new_block ()];
    screen = screen_init White;
    num_lines_removed = 0;
    num_lines_multiple = 0;
    score = 0;
    score_multiple = 0;
    refresh = true;
    timer = gettimeofday ();
    held_block = None;
    can_swap = true;
    hit_timer = gettimeofday ();
    hit_counter = 0;
    game_state = Inactive;
    user_name = !user_name;
  }

(*********** Parse Input ************) 
(** [parse_input state] is the game state [state] with updated 
    block coordinates based on user input. *)
let parse_input_active state =
  match read_key() with
  | 'w' -> move_tetro_down_hard {state with refresh = true} 
  | 'a' -> move_block_left {state with refresh = true}
  | 's' -> move_block_down_soft {state with refresh = true} 
  | 'd' -> move_block_right {state with refresh = true}
  | 'q' -> rotate_ccw {state with refresh = true}
  | 'e' -> rotate_cw {state with refresh = true}
  | ' ' -> if state.can_swap then swap_with_held_block state else state
  | x -> set_color magenta; state

(** [parse_pause_active ()] is true when the pause button is pressed while
    the current state is active. *)
let parse_pause_active () = 
  parse_button ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*22)
    ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*3)
    (square_size*3) (square_size*8)

(** [parse_input_settings ()] is the game state if the user chooses to save 
    a change configuration of settings. *)
let parse_input_settings state = 
  if (button_down ()) then 
    make_change (mouse_pos ()) state
  else state

(** [update state] is the game state [state] with updated information 
    after one time increment. *)
let rec update state =
  match state.game_state with 
  | Active -> update_active state
  | Settings -> update_settings state
  | Inactive -> update_inactive state 
  | Pause -> update_pause state
  | Leaderboard -> update_leaderboard state 
  | Tutorial -> update_tutorial state

(** [hit_bottom_update state] is the game state [state] with updated 
information when the current block hits the bottom of the screen in the 
[Active] state. *)
and hit_bottom_update state = 
  if state.hit_counter = 0 then 
    {state with hit_counter = 1; hit_timer = gettimeofday ()} |> update

  else if state.hit_counter = 1 then begin
    if gettimeofday () -. state.hit_timer > time_moving_after_hit then 
      {state with hit_counter = 3} |> update 
    else (
      let event = Graphics.wait_next_event [ Graphics.Poll ] in
      if event.Graphics.keypressed then (
        let new_state = parse_input_active state in 
        if (hit_bottom new_state) then new_state |> update
        else {new_state with hit_counter = 0} |> update) 
      else update state)
  end
  else begin 
    (if state.cur_block.color = Freeze then 
       freeze drop_time 2 else 
       defreeze drop_time 2); 
    {state with hit_counter = 0} |> score_lines |> update
  end

(** [update_active state] is the game state [state] with updated information 
    after one time increment while the current state is [Active]. *)
and update_active state = 
  if state.refresh then (
    clear_graph ();
    if !is_rect then draw_active_screen_rec state
    else draw_active_screen_round state;
    update {state with refresh = false}) 

  else if (parse_pause_active ()) then 
    update {state with game_state = Pause; refresh = true}

  else if (hit_bottom state) then hit_bottom_update state

  else let event = Graphics.wait_next_event [ Graphics.Poll ] in
    if event.Graphics.keypressed then (
      parse_input_active state |> update) 
    else if (((gettimeofday ()) -. state.timer) > !drop_time) then (
      move_block_down_soft 
        {state with refresh = true; timer = gettimeofday ()} |> update)
    else update state

(** [update_inactive state] is the game state [state] with updated information 
    after one time increment while the current state is [Inactive]. *)
and update_inactive state = 
  if state.refresh then
    (
      clear_graph (); 
      draw_inactive_screen ();
      (match !difficulty with 
       | Easy -> drop_time := 2.5; base_time := 2.5
       | Medium -> drop_time := 2.0; base_time := 2.0
       | Hard -> drop_time := 1.5; base_time := 1.5);
      let init_st = init_state () in 
      {init_st with refresh = false} |> update
    ) 
  else if (parse_start_inactive ()) then 
    update {state with game_state = Active; refresh = true}
  else if (parse_tutorial_inactive ()) then 
    update {state with game_state = Tutorial; refresh = true}
  else if (parse_leaderboard_inactive ()) then 
    update {state with game_state = Leaderboard; refresh = true}
  else if (parse_settings_inactive ()) then 
    update {state with game_state = Settings; refresh = true}
  else if ((same_button ()) = false) then 
    update {state with refresh = true}
  else update state

(** [update_pause state] is the game state [state] with updated information 
    after one time increment while the current state is [Pause]. *)
and update_pause state = 
  if state.refresh then
    (clear_graph (); 
     draw_pause_screen ();
     {state with refresh = false} |> update
    )
  else if (parse_exit_pause ()) then 
    update {state with game_state = Inactive; refresh = true}
  else if (parse_resume_pause ()) then  
    update {state with game_state = Active; refresh = true}
  else update state

(** [update_settings state] is the game state [state] with updated information 
    after one time increment while the current state is [Settings]. *)
and update_settings state = 
  let event = Graphics.wait_next_event [ Graphics.Poll ] in
  if state.refresh then 
    (clear_graph (); draw_settings_screen state;
     (update ({state with refresh = false})))
  else
    (if event.Graphics.button 
     then parse_input_settings state |> update
     else update state)

(** [update_tutorial state] is the game state [state] with updated information 
    after one time increment while the current state is [Tutorial]. *)
and update_tutorial state = 
  if state.refresh then 
    (clear_graph ();
     draw_tutorial_screen ();
     {state with refresh = false} |> update
    )
  else if (parse_exit_tutorial ()) then 
    update {state with game_state = Inactive; refresh = true}
  else update state

(** [update_leaderboard state] is the game state [state] with updated 
information after one time increment while the current state is [Leaderboard].
 *)
and update_leaderboard state = 
  if state.refresh then
    (
      clear_graph (); 
      renew_leaderboard state |> draw_leaderboard |> update
    )
  else if (parse_exit_leaderboard ()) then (init_state () |> update)
  else update state

(** [()] initializes thw game window, game state, as well as the game loop. *)
let () = parse_variables (); init_window (); init_state () |> update

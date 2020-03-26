(* HIGH LEVEL TEST PLAN: 
   In this top comment here, we documented a high level test plan explaining how
   we tests every module. Because it is game that we are implementing, we used 
   both "Ounit tests" and "Interactive tests" (i.e. play the game). 

   Modules that uses "Ounit tests" includes:
   movetetro; 
   gamesetup (swap and powerup);
   leaderboard;
   scoreline;
   settings
   Overall we used mainly black box testing in Ounit tests. However, for some
   functions we did switch to white box testing just so we can test out each
   branch/possibility with target. For each module above, we have written a 
   detailed test plan below in corresponding section, detailing how exactly 
   we developed test cases and why they prove the correctness of the system. I
   will omit the detail in this high-level plan for that reason. 
  
   Modules that uses "Interactive tests" includes: (Rationale attached)
   All drawing modules and game_state transition modules. 
   We decided to use interactive tests for these modules because these modules 
   relys on GUI heavily. Therefore, it is only possible to test them by
   directly playing the game.

   We believe that we have throughly tested the system for two reasons. 
   1) Features related to math/game itself are tested by Ounit test. Examples 
   include move tetro piece, game over condition...
   2) Features related to visualization/ mouse/ keyboard are throughly tested 
   by interactive playing. 
   Therefore, we have full confidence in the system!
*)

open OUnit2
open Unix 
open Gamesetup
open Movetetro

(***************************** Move Tetro *******************************)  
(* TEST PLAN: 

   To test the functions in movetetro, we hardcoded several screens 
   with blocks in set locations. In order to act upon blocks, we made
   states. The most important aspects of those states were the cur_block, which
   represented the block that was currently falling, and the screen containing
   the blocks which had already fallen. Nothing else in the states had any
   relation to the movetetro functions, but needed to be included to satisfy
   the requirements of the state record. 

   We first ensured that the horizontal and vertical movement functions 
   (move_block_down_soft, move_tetro_down_hard, move_block_left, 
   move_block_right) worked on the O block. Because the O block is shaped 
   like a square, it is simplest to move, and we did not have to worry about 
   rotating it. 

   After ensuring that the O block could be moved properly, we repeated the 
   same movement tests on the S block. Because of the S block's more varied
   shape compared to the O block, we felt confident that tests on this block in 
   addition to the O block would be enough to show that translational movement 
   functions worked for any tetris piece regardless of shape. 

   To test clockwise and counterclockwise rotation (rotate_cw and rotate_ccw),
   we used the S block and the L block. The S block has only two possible 
   orientations, while the L block has four. For both blocks, we had to 
   consider if/where their origins would be repositioned upon rotating. 
   We also tested a couple of rotation tests for the I block because we had 
   been experiencing a glitch where the size of the I block would decrease upon 
   rotating counterclockwise. The bug was fixed quickly enough that we didn't
   see a need to do any more tests on the I block.

   OUnit tests showed that specific blocks worked fine, and we trusted that the
   same would be true of the other blocks. Just to be sure, we ALSO tested by
   manualing moving randomly generated blocks in the GAME WINDOW. We were able 
   to see that all blocks moved as desired, so we felt that our testing was
   sufficient.

   For the most part, our process relied on BLACK BOX testing rather than 
   glass box testing. We occasionally looked at how the functions were 
   implemented, but for almost all tests, we estimated the block positions by 
   drawing the expected results, then wrote the matching test case.
*)
let rec rotate_cw_helper (lst : (int * int) list) =
  match lst with 
  | [] -> []
  | (x, y) :: t -> (y, -x) :: (rotate_cw_helper t)

let rec rotate_ccw_helper (lst : (int * int) list) =
  match lst with 
  | [] -> []
  | (x, y) :: t -> (-y, x) :: (rotate_ccw_helper t)

let sorted_pixels (lst : (int * int) list) =
  List.sort Stdlib.compare (lst) 

let screen1 = Array.make_matrix 20 10 White 

(* screen with fallen o block *)
let screen2 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(2) <- Yellow;
  org.(18).(3) <- Yellow;
  org.(19).(2) <- Yellow;
  org.(19).(3) <- Yellow;
  org

(* screen with fallen l block *)
let screen3 = 
  let org = Array.make_matrix 20 10 White in 
  org.(19).(1) <- Orange;
  org.(19).(2) <- Orange;
  org.(19).(3) <- Orange;
  org.(18).(3) <- Orange;
  org

(* screen with fallen o block in bottom left corner *)
let screen4 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(0) <- Yellow;
  org.(19).(0) <- Yellow;
  org.(18).(1) <- Yellow;
  org.(19).(1) <- Yellow;
  org

(* screen with fallen o block in bottom right corner *)
let screen5 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(8) <- Yellow;
  org.(18).(9) <- Yellow;
  org.(19).(8) <- Yellow;
  org.(19).(9) <- Yellow;
  org

let i_block = new_I_block (2, 4)
let j_block = new_J_block (0, 7)
let l_block = new_L_block (2, 6)
let o_block = new_O_block (2, 6)
let s_block = new_S_block (2, 6)
let z_block = new_Z_block (2, 1)
let t_block = new_T_block (0, 7)

(****** O block ******) 

let left_o_block = new_O_block (1, 5)
let right_o_block = new_O_block (3, 6)
let down_o_block = new_O_block (2, 5)
let lower_o_block = new_O_block (3, 3)
(* o block that can't move left due to fallen o block *)
let no_left_o_block = new_O_block (4, 3)
(* o block that can't move right due to fallen o block *)
let no_right_o_block = new_O_block (0, 3)
(* o block on the bottom of the screen *)
let fallen_o_block = new_O_block (2, 2)
(* o block - no collision *)
let org_o_block_state = {
  cur_block = o_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let left_o_block_state = {
  cur_block = left_o_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let right_o_block_state = {
  cur_block = right_o_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let down_o_block_state = {
  cur_block = down_o_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* o block - touching left edge *)
let left_edge_org_o_block_state = {
  cur_block = {(new_O_block (0, 6)) with pos = (0, 5)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let left_edge_down_o_block_state = {
  cur_block = {(new_O_block (0, 5)) with pos = (0, 4)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* o block - touching right edge *)
let right_edge_org_o_block_state = {
  cur_block = {(new_O_block (8, 6)) with pos = (8, 5)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let right_edge_down_o_block_state = {
  cur_block = {(new_O_block (8, 5)) with pos = (8, 4)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let no_left_o_block_state = {
  cur_block = no_left_o_block;
  screen = screen2;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let no_right_o_block_state = {
  cur_block = no_right_o_block;
  screen = screen2;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let fallen_l_block = {(new_L_block (2, 2)) with pos = (2, 0)}

(* o block collides with fallen l block*)
let down_o_l_block_state = {
  cur_block = lower_o_block;
  screen = screen3;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}



(****** S block ******) 

let left_s_block = {(new_S_block(1, 6)) with pos = (1, 5)}
let right_s_block = {(new_S_block(3, 6)) with pos = (3, 5)}
let down_s_block = {(new_S_block(2, 5)) with pos = (2, 4)}
let top_s_block = {(new_S_block(5, 20)) with pos = (5, 19)}

let lower_s_block = {(new_S_block(4, 4)) with pos = (4, 3)}
(* s block that can't move left due to fallen l block *)
let no_left_s_block = {(new_S_block(5, 3)) with pos = (5, 2)}
(* s block that can't move right due to fallen l block *)
let no_right_s_block = {(new_S_block(2, 3)) with pos = (2, 2)}
(* s block on the bottom of the screen *)
let fallen_s_block = {(new_S_block(2, 2)) with pos = (2,1)}

(* s block - no collision *)
let org_s_state = {
  cur_block = s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let left_s_state = {
  cur_block = left_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let right_s_state = {
  cur_block = right_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let down_s_state = {
  cur_block = down_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let top_s_state = {
  cur_block = top_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* s block - touching left edge *)
let left_edge_org_s_block_state = {
  cur_block = {(new_S_block(1,6)) with pos = (1, 5)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let left_edge_down_s_block_state = {
  cur_block = {(new_S_block(1,5)) with pos = (1, 4)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* s block - touching right edge *)
let right_edge_org_s_block_state = {
  cur_block = {(new_S_block(8, 6)) with pos = (8, 5)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let right_edge_down_s_block_state = {
  cur_block = {(new_S_block(8, 5)) with pos = (8, 4)};
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let no_left_s_block_state = {
  cur_block = no_left_s_block;
  screen = screen3;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let no_right_s_block_state = {
  cur_block = no_right_s_block;
  screen = screen3;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let down_s_l_block_state = {
  cur_block = lower_s_block;
  screen = screen3;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_cw_s_block = 
  {(new_S_block(8, 6)) with pixels = [(0, 0); (0, -1); (-1, 1); (-1, 0)]}
let rotate_ccw_s_block = 
  {(new_S_block(8, 6)) with pixels = [(0, 0); (0, 1); (1, -1); (1, 0)]}

let rotate_cw_s_state = {
  cur_block = rotate_cw_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_ccw_s_state = {
  cur_block = rotate_ccw_s_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}



(****** L block ******) 

let snd_l_block = {(new_L_block(2, 6)) with pos = (2, 4)}
let rotate_cw_l_block = 
  {(new_L_block(2, 6)) with pixels = [(1, -1); (0, 1); (0, 0); (0, -1)]}
let rotate_ccw_l_block = 
  {(new_L_block(2, 6)) with pixels = [(-1, 1); (0, -1); (0, 0); (0, 1)]}
let no_movement_l_block = 
  {(new_L_block(2, 6)) with pos = (3,1)}

(* l block - no collision *)
let org_l_state = {
  cur_block = l_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let snd_l_state = {
  cur_block = snd_l_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let fallen_l_state = {
  cur_block = fallen_l_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* state with o block in bottom left corner and falling l block *)
let bl_o_l_state = {
  cur_block = no_movement_l_block;
  screen = screen4;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(* state with o block in bottom right corner and falling l block *)
let br_o_l_state = {
  cur_block = no_movement_l_block;
  screen = screen5;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_cw_l_state = {
  cur_block = rotate_cw_l_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_ccw_l_state = {
  cur_block = rotate_ccw_l_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

(****** I block ******) 

let rotate_cw_i_block = 
  {(new_I_block(5, 5)) with pixels = [(0,1); (0,0); (0,-1); (0,-2)]}
let rotate_ccw_i_block = 
  {(new_I_block(5, 5)) with pixels = [(0,-1); (0,0); (0,1); (0,2)]}

let org_i_state = {
  cur_block = i_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_cw_i_state = {
  cur_block = rotate_cw_i_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let rotate_ccw_i_state = {
  cur_block = rotate_ccw_i_block;
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  queue = [new_block (); new_block (); new_block ()];
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let move_down_soft_tests = [
  "move down org_o_block_state" >:: (fun _ -> 
      assert_equal 
        (move_block_down_soft org_o_block_state).cur_block.pos 
        down_o_block.pos);
  "move down left_edge_o_block_state" >:: (fun _ -> 
      assert_equal 
        (move_block_down_soft left_edge_org_o_block_state).cur_block.pos 
        left_edge_down_o_block_state.cur_block.pos);
  "move down right_edge_o_block_state" >:: (fun _ -> 
      assert_equal 
        (move_block_down_soft right_edge_org_o_block_state).cur_block.pos
        right_edge_down_o_block_state.cur_block.pos);

  "move down o_block l_block collision" >:: (fun _ -> 
      assert_equal 
        (move_block_down_soft down_o_l_block_state).cur_block.pos 
        down_o_l_block_state.cur_block.pos);

  "move down s_block no collision" >:: (fun _ -> 
      assert_equal 
        (move_block_down_soft org_s_state).cur_block.pos 
        down_s_state.cur_block.pos);
  "move down s_block touching left edge" >:: (fun _ ->
      assert_equal 
        (move_block_down_soft left_edge_org_s_block_state).cur_block.pos
        left_edge_down_s_block_state.cur_block.pos);
  "move down s_block touching right edge" >:: (fun _ ->
      assert_equal 
        (move_block_down_soft right_edge_org_s_block_state).cur_block.pos
        right_edge_down_s_block_state.cur_block.pos);
  "move down s_block l_block collision" >:: (fun _ -> 
      assert_equal (move_block_down_soft down_s_l_block_state).cur_block.pos 
        down_s_l_block_state.cur_block.pos)

]

let move_down_hard_tests = [
  "hard drop on empty screen" >:: (fun _ -> 
      assert_equal (move_tetro_down_hard org_o_block_state).cur_block.pos 
        fallen_o_block.pos);
]

let move_left_tests = [
  "move left o_block no collision" >:: (fun _ ->
      assert_equal (move_block_left org_l_state).cur_block.pos
        left_o_block.pos);
  "move left o_block touching left edge" >:: (fun _ ->
      assert_equal (move_block_left left_edge_org_o_block_state).cur_block.pos
        left_edge_org_o_block_state.cur_block.pos);
  "move left o_block colliding with block" >:: (fun _ ->
      assert_equal (move_block_left no_left_o_block_state).cur_block.pos
        no_left_o_block_state.cur_block.pos);

  "move left s_block no collision" >:: (fun _ ->
      assert_equal (move_block_left org_s_state).cur_block.pos
        left_s_state.cur_block.pos);
  "move left s_block touching left edge" >:: (fun _ ->
      assert_equal (move_block_left left_edge_org_s_block_state).cur_block.pos
        left_edge_org_s_block_state.cur_block.pos);
  "move left s_block colliding with block" >:: (fun _ ->
      assert_equal (move_block_left no_left_s_block_state).cur_block.pos
        no_left_s_block_state.cur_block.pos);
]

let move_right_tests = [
  "move right o_block no collision" >:: (fun _ ->
      assert_equal (move_block_right org_o_block_state).cur_block.pos
        right_o_block.pos);
  "move right o_block touching right edge" >:: (fun _ ->
      assert_equal (move_block_right right_edge_org_o_block_state).cur_block.pos
        right_edge_org_o_block_state.cur_block.pos);
  "move right o_block colliding with block" >:: (fun _ ->
      assert_equal (move_block_right no_right_o_block_state).cur_block.pos
        no_right_o_block_state.cur_block.pos);

  "move right s_block no collision" >:: (fun _ ->
      assert_equal (move_block_right org_s_state).cur_block.pos
        right_s_state.cur_block.pos);
  "move right s_block touching right edge" >:: (fun _ ->
      assert_equal (move_block_right right_edge_org_s_block_state).cur_block.pos
        right_edge_org_s_block_state.cur_block.pos);
  "move right s_block colliding with block" >:: (fun _ ->
      assert_equal (move_block_right no_right_s_block_state).cur_block.pos
        no_right_s_block_state.cur_block.pos);
]

let rotate_tests = [
  "rotate_cw s_block" >:: (fun _ -> 
      assert_equal (rotate_cw_helper no_right_s_block_state.cur_block.pixels ) 
        (rotate_cw no_right_s_block_state).cur_block.pixels);
  "s_block that can't be rotated cw" >:: (fun _ ->
      assert_equal top_s_state.cur_block.pixels
        (rotate_cw top_s_state).cur_block.pixels);
  "s_block that collides with a block but can be rotated cw" >:: (fun _ ->
      assert_equal (rotate_cw_helper no_left_s_block_state.cur_block.pixels)
        (rotate_cw no_left_s_block_state).cur_block.pixels);

  "rotate_ccw s_block" >:: (fun _ -> 
      assert_equal ( rotate_ccw_helper down_s_state.cur_block.pixels ) 
        rotate_ccw_s_block.pixels);
  "s_block that can't be rotated ccw" >:: (fun _ ->
      assert_equal top_s_state.cur_block.pixels
        (rotate_ccw top_s_state).cur_block.pixels);
  "s_block that collides with a block but can be rotated ccw" >:: (fun _ ->
      assert_equal 
        (sorted_pixels 
           (rotate_ccw_helper no_left_s_block_state.cur_block.pixels))
        (sorted_pixels rotate_ccw_s_block.pixels));

  "rotate_cw l_block" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_cw_helper snd_l_state.cur_block.pixels))
        (sorted_pixels rotate_cw_l_block.pixels));
  "l_block that can't be rotated cw" >:: (fun _ ->
      assert_equal (rotate_cw_helper fallen_l_state.cur_block.pixels)
        rotate_cw_l_block.pixels);
  "l_block that collides with a block but can be rotated cw" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_cw_helper bl_o_l_state.cur_block.pixels))
        (sorted_pixels rotate_cw_l_block.pixels));

  "rotate_ccw l_block" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_ccw_helper org_l_state.cur_block.pixels)) 
        (sorted_pixels rotate_ccw_l_block.pixels));
  "l_block that can't be rotated ccw" >:: (fun _ ->
      assert_equal (rotate_ccw_helper fallen_l_state.cur_block.pixels)
        rotate_ccw_l_block.pixels);
  "l_block that collides with a block but can be rotated ccw" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_ccw_helper br_o_l_state.cur_block.pixels))
        (sorted_pixels rotate_ccw_l_block.pixels));
]

let i_tests = [

  "rotate_cw i_block" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_cw_helper org_i_state.cur_block.pixels))
        (sorted_pixels rotate_cw_i_block.pixels));

  "rotate_ccw i_block" >:: (fun _ ->
      assert_equal 
        (sorted_pixels (rotate_ccw_helper org_i_state.cur_block.pixels)) 
        (sorted_pixels rotate_ccw_i_block.pixels));
]

(************************* GAMESETUP (SWAP) *****************************)
(** Test Plan:
    For swappinhg tests:
    We tested the swapping feature below using white box testing. When the swap
    function is called (i.e. is allowed to swap), there are two cases: 1, there
    is an existing block to be swapped with, and 2, when there isn't. For both
    cases, we only test the fields in state that are affected by the swap 
    (cur_block, held_block, queue, can_swap) as opposed to comparing the whole 
    state, since only a few fields are affected (which is why we chose the
    white box method).
*)

let screen_init color = Array.make_matrix !screen_height !screen_width color

let state_held_empty = {
  cur_block = new_L_block (!start_pos_x, !start_pos_y);
  queue = [new_J_block (!start_pos_x, !start_pos_y); 
           new_L_block (!start_pos_x, !start_pos_y); 
           new_O_block (!start_pos_x, !start_pos_y)];
  screen = screen_init White;
  score = 0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  refresh = true;
  timer = gettimeofday ();
  hit_timer = gettimeofday ();
  hit_counter = 0;
  game_state = Inactive;
  user_name = !user_name;
  held_block = None;
  can_swap = true;
}

let state_swapped_held_empty = {
  cur_block = new_J_block (!start_pos_x, !start_pos_y);
  queue = [new_J_block (!start_pos_x, !start_pos_y); 
           new_L_block (!start_pos_x, !start_pos_y); 
           new_O_block (!start_pos_x, !start_pos_y)];
  screen = screen_init White;
  score = 0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  refresh = true;
  timer = gettimeofday ();
  hit_timer = gettimeofday ();
  hit_counter = 0;
  game_state = Inactive;
  user_name = !user_name;
  held_block = Some (new_L_block (!start_pos_x, !start_pos_y));
  can_swap = true;
}

let state_held_not_empty = {
  cur_block = new_L_block (!start_pos_x, !start_pos_y);
  queue = [new_J_block (!start_pos_x, !start_pos_y); 
           new_L_block (!start_pos_x, !start_pos_y); 
           new_O_block (!start_pos_x, !start_pos_y)];
  screen = screen_init White;
  score = 0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  refresh = true;
  timer = gettimeofday ();
  hit_timer = gettimeofday ();
  hit_counter = 0;
  game_state = Inactive;
  user_name = !user_name;
  held_block = Some (new_I_block (!start_pos_x, !start_pos_y));
  can_swap = false;
}

let state_swapped_held_not_empty = {
  cur_block = new_I_block (!start_pos_x, !start_pos_y);
  queue = [new_J_block (!start_pos_x, !start_pos_y); 
           new_L_block (!start_pos_x, !start_pos_y); 
           new_O_block (!start_pos_x, !start_pos_y)];
  screen = screen_init White;
  score = 0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  refresh = true;
  timer = gettimeofday ();
  hit_timer = gettimeofday ();
  hit_counter = 0;
  game_state = Inactive;
  user_name = !user_name;
  held_block = Some (new_L_block (!start_pos_x, !start_pos_y));
  can_swap = false;
}

let swap_tests = [
  "swapping new block successfully, checks held block" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_empty).held_block 
      state_swapped_held_empty.held_block);
  "swapping new block successfully, checks cur block from queue" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_empty).cur_block
      (match state_swapped_held_empty.queue with
       | h ::t -> h
       | [] -> failwith "Not happening"));
  "swapping new block successfully, checks can_swap" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_empty).can_swap false);
  "swapped with non-empty held, checks held block" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_not_empty).held_block 
      state_swapped_held_not_empty.held_block);
  "swapped with non-empty held, checks current block" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_not_empty).cur_block 
      state_swapped_held_not_empty.cur_block);
  "swapped with non-empty held, checks queue" >:: 
  (fun _ -> assert_equal 
      (swap_with_held_block state_held_not_empty).queue 
      state_swapped_held_not_empty.queue);
]

(************************* GAMESETUP POWERUP ***************************)  
(* TEST PLAN: 

   We tested the laser bomb and freeze power-ups using glass box testing. For 
   the bomb, we knew that when a bomb lands, it should eliminate the tetros 
   which lie on the same row as it. The laser bomb should not eliminate any 
   other rows. For the freeze power-up, we need to be mindful of how the speed 
   changes as the game progresses. Freezing should slow down a block, but then 
   restore the drop time to its original speed after it lands. Regardless of 
   how many times the freeze power-up is called, the drop rate always returns 
   to its regular rate, whatever that currently is, and does not become 
   faster than that. In addition to the OUnit tests, we played the game and
   saw the desired functionality, so we are certain that the power-ups work
   as they should.

*)
open OUnit2
open Unix 
open Gamesetup
open Scoreline
open Movetetro

let slow_drop_time = ref 3.0

(* empty screen *)
let screen1 = Array.make_matrix 20 10 White 

(* screen with fallen o block *)
let screen2 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(2) <- Yellow;
  org.(18).(3) <- Yellow;
  org.(19).(2) <- Yellow;
  org.(19).(3) <- Yellow;
  org

(* screen with fallen l block *)
let screen3 = 
  let org = Array.make_matrix 20 10 White in 
  org.(19).(1) <- Orange;
  org.(19).(2) <- Orange;
  org.(19).(3) <- Orange;
  org.(18).(3) <- Orange;
  org

(* screen with fallen o block in bottom left corner *)
let screen4 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(0) <- Yellow;
  org.(19).(0) <- Yellow;
  org.(18).(1) <- Yellow;
  org.(19).(1) <- Yellow;
  org

(* screen with fallen o block in bottom right corner *)
let screen5 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(8) <- Yellow;
  org.(18).(9) <- Yellow;
  org.(19).(8) <- Yellow;
  org.(19).(9) <- Yellow;
  org

(* screen with two Yellow pixels in bottom right corner *)
let screen6 = 
  let org = Array.make_matrix 20 10 White in 
  org.(19).(8) <- Yellow;
  org.(19).(9) <- Yellow;
  org

(* screen with an I block on top of an O block in the bottom right corner *)
let screen7 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(8) <- Yellow;
  org.(18).(9) <- Yellow;
  org.(19).(8) <- Yellow;
  org.(19).(9) <- Yellow;
  org.(17).(9) <- Cyan;
  org.(16).(9) <- Cyan;
  org.(15).(9) <- Cyan;
  org.(14).(9) <- Cyan;
  org

(* screen with 3 pixels of an I block on top of an O block in the bottom right 
   corner *)
let screen8 = 
  let org = Array.make_matrix 20 10 White in 
  org.(18).(8) <- Yellow;
  org.(18).(9) <- Yellow;
  org.(19).(8) <- Yellow;
  org.(19).(9) <- Yellow;
  org.(17).(9) <- Cyan;
  org.(16).(9) <- Cyan;
  org.(15).(9) <- Cyan;
  org

let bomb = new_bomb_block (3,2)
let stacked_bomb = new_bomb_block (8, 3)
let freeze_block = new_freeze_block (3,0)

let block1 = {
  pos = (2, 2);
  pixels = [(0, 0); (-1, 0); (1, 0); (0, 1)];
  color = Red
}

let org_bomb_state = {
  cur_block = bomb;
  queue = [block1; block1; block1]; 
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let row_delete_bomb_state = {
  cur_block = bomb;
  queue = [block1; block1; block1]; 
  screen = screen5;
  score = 0;
  refresh = false;
  timer = 1.0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let row_delete_color_shift_bomb_state = {
  cur_block = stacked_bomb;
  queue = [block1; block1; block1]; 
  screen = screen7;
  score = 0;
  refresh = false;
  timer = 1.0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let org_freeze_state = {
  cur_block = freeze_block;
  queue = [block1; block1; block1]; 
  screen = screen1;
  score = 0;
  refresh = false;
  timer = 1.0;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score_multiple = 0;
  held_block = None;
  can_swap = false;
  game_state = Settings;
  user_name = "x";
  hit_timer = 0.0;
  hit_counter = 1;
}

let bomb_tests = [

  "bomb lands and eliminates blocks in row" >:: (fun _ -> 
      let new_state = (move_block_down_soft 
                         (move_tetro_down_hard row_delete_bomb_state)) in
      let new_new_state = laser_bomb new_state in
      assert_equal 
        (new_new_state).screen
        screen6);

  "bomb lands in empty row and results in empty row" >:: (fun _ ->
      assert_equal (move_tetro_down_hard org_bomb_state).screen
        screen1);

  "bomb only eliminates tetros in its row, not in those above or below it" 
  >:: (fun _ ->
      let new_state = 
        (move_block_down_soft 
           (move_tetro_down_hard row_delete_color_shift_bomb_state)) in
      let new_new_state = laser_bomb new_state in
      assert_equal (new_new_state).screen screen8);
]

let freeze_tests = [

  "freeze slows the drop time" >:: (fun _ -> 
      freeze drop_time 2; 
      assert_equal !drop_time 3.0);
  "multiple freezes slow the falling rate multiple times" >:: (fun _ -> 
      freeze drop_time 2;
      freeze drop_time 2;
      assert_equal !drop_time 6.0);

  "defreeze speeds up the drop time" >:: (fun _ ->
      defreeze (slow_drop_time) 2;
      assert_equal !slow_drop_time 1.5);

  "multiple defreezes will not speed up the block faster than its 
     original falling rate" >:: (fun _ ->
      defreeze (slow_drop_time) 2;
      defreeze (slow_drop_time) 2;
      defreeze (slow_drop_time) 2;
      assert_equal !slow_drop_time 1.5);

]


(************************* SCORELINE *************************)
(* For game over checks:
    We tested our checks for game over by testing several scenarios, which are
    generally in two cases: when there are objects overlapping the new spawned
    block (which means game over), and when there isn't (game progresses). We
    used the white box testing method and simply checked if our game over flag
    [game_over_collides] returns true or false in each of these cases. *)

open Scoreline

let empty_screen = Array.make_matrix 6 7 White

let below_filled_screen = 
  let org = Array.make_matrix 6 7 White in 
  org.(2).(0) <- Yellow;
  org.(2).(1) <- Yellow;
  org.(2).(2) <- Yellow;
  org.(2).(3) <- Yellow;
  org.(2).(4) <- Yellow;
  org.(2).(5) <- Yellow;
  org.(2).(6) <- Yellow;
  org

let filled_screen =
  let org = Array.make_matrix 6 7 White in 
  org.(2).(0) <- Yellow;
  org.(2).(1) <- Yellow;
  org.(2).(2) <- Yellow;
  org.(2).(3) <- Yellow;
  org.(2).(4) <- Yellow;
  org.(2).(5) <- Yellow;
  org.(2).(6) <- Yellow;
  org.(1).(0) <- Yellow;
  org.(1).(1) <- Yellow;
  org.(1).(2) <- Yellow;
  org.(1).(3) <- Yellow;
  org.(1).(4) <- Yellow;
  org.(1).(5) <- Yellow;
  org.(1).(6) <- Yellow;
  org

let game_over_tests = [
  "line below generated block is filled" >:: (fun _ -> 
      assert_equal (game_over_collides below_filled_screen 
                      (new_L_block (!start_pos_x, !start_pos_y))) true);
  "generated block overlaps with blocks on screen" >:: (fun _ -> 
      assert_equal (game_over_collides filled_screen 
                      (new_O_block (!start_pos_x, !start_pos_y))) true);
]

(***************************** Leaderboardcsv ********************************)
(* TEST PLAN:
   For Leaderboardcsv: 
    The file leaderboardcsv.ml handles the logic behind 
    keeping track of the leaderboard. It contains 9 functions; 3 functions are
    tested below, and 8 are tested manually, with reasons as elaborated below.

    The functions game_over_score_info, add_cur_score_info, and 
    update_top_score_info are tested by OUnit below. For add_cur_score_info and 
    game_over_score_info, we implemented a black box testing approach, as 
    the values returned should be relatively straightforward; there are no edge 
    cases. As for update_top_score_info, the tests below cover edge cases for 
    sorting and returning the top 10 high scorers' information (if they exist).

    The remaining 5 functions had to be tested manually as they involve I/O: all
    5 functions involve reading and writing scoring informaton onto the csv file
    [leaderboard.csv]. The approach we took was to test reading/writing scorer 
    information with [read_from_csv] and [write_to_csv] in utop, then checking
    that the correct format of data was written onto the csv. We also tested
    [get_tuple_lst] on utop to check that the list of score info returned is
    consistent with what was in the csv file.

    Lastly, we tested the remaining 3 functions by using make play. This is
    because the remaining functions are primarily involved in drawing the stored
    data on the graphics display; hence we tested [renew_leaderboard] and 
    [draw_leaderboard] by manually checking the game's leaderboard update when
    we finish playing a game. We also tested [parse_exit_leaderboard] in make 
    play by checking the state transitions are correct. *)
open Leaderboardcsv

let go_state = {
  cur_block = new_block ();
  queue = [new_block (); new_block (); new_block ()];
  screen = screen_init White;
  num_lines_removed = 0;
  num_lines_multiple = 0;
  score = 100;
  score_multiple = 0;
  refresh = true;
  timer = gettimeofday ();
  held_block = None;
  can_swap = true;
  hit_timer = gettimeofday ();
  hit_counter = 0;
  game_state = Inactive;
  user_name = "Michael Clarkson";
}

let todays_date () = 
  let time_rec = localtime (time ()) in
  let day = time_rec.tm_mday in
  let month = time_rec.tm_mon in
  let year = (time_rec.tm_year + 1900) in
  (string_of_int (day))^"/"^(string_of_int (month+1))^"/"^(string_of_int year)


let leaderboard_tests = [
  "empty list, no scores" >:: (fun _ -> 
      tuple_lst := [];
      update_top_score_info ();
      assert_equal (!tuple_lst) ([]));

  "all scores equal, sorting has no effect" >:: (fun _ -> 
      tuple_lst := [("Alpha",10,"9/12/2019"); ("Beta",10,"9/12/2019"); 
                    ("Gamma",10,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",10,"9/12/2019"); ("Beta",10,"9/12/2019"); 
          ("Gamma",10,"9/12/2019")]));

  "all scores sorted, sorting has no effect" >:: (fun _ -> 
      tuple_lst := [("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
                    ("Gamma",10,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
          ("Gamma",10,"9/12/2019")]));

  "all scores sorted, dates have no effect" >:: (fun _ -> 
      tuple_lst := [("Alpha",30,"9/12/2019"); ("Beta",20,"10/12/2019"); 
                    ("Gamma",10,"11/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",30,"9/12/2019"); ("Beta",20,"10/12/2019"); 
          ("Gamma",10,"11/12/2019")]));

  "one score unsorted" >:: (fun _ -> 
      tuple_lst := [("Alpha",30,"9/12/2019"); ("Gamma",10,"9/12/2019"); 
                    ("Beta",20,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
          ("Gamma",10,"9/12/2019")]));

  "all scores not in order" >:: (fun _ -> 
      tuple_lst := [("Gamma",10,"9/12/2019"); ("Beta",20,"9/12/2019"); 
                    ("Alpha",30,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
          ("Gamma",10,"9/12/2019")]));

  "score info list with more than 10 scores (only considers the 10 elements)" 
  >:: (fun _ -> 
      tuple_lst := [("Alpha",10,"9/12/2019"); ("Beta",10,"9/12/2019"); 
                    ("Gamma",10,"9/12/2019"); ("Delta",10,"9/12/2019"); 
                    ("Epsilon",10,"9/12/2019"); ("Alpha",10,"9/12/2019");
                    ("Beta",10,"9/12/2019"); ("Gamma",10,"9/12/2019"); 
                    ("Delta",10,"9/12/2019"); ("Epsilon",10,"9/12/2019");
                    ("Lambda",10,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",10,"9/12/2019"); ("Beta",10,"9/12/2019"); 
          ("Gamma",10,"9/12/2019"); ("Delta",10,"9/12/2019"); 
          ("Epsilon",10,"9/12/2019"); ("Alpha",10,"9/12/2019"); 
          ("Beta",10,"9/12/2019"); ("Gamma",10,"9/12/2019"); 
          ("Delta",10,"9/12/2019"); ("Epsilon",10,"9/12/2019")]));

  "unsorted score info list with more than 10 scores only considers the top 10" 
  >:: (fun _ -> 
      tuple_lst := [("Alpha",100,"9/12/2019"); ("Beta",90,"9/12/2019");
                    ("Gamma",80,"9/12/2019"); ("Delta",70,"9/12/2019"); 
                    ("Epsilon",60,"9/12/2019"); ("Alpha",50,"9/12/2019"); 
                    ("Beta",40,"9/12/2019"); ("Gamma",30,"9/12/2019"); 
                    ("Delta",20,"9/12/2019"); ("Epsilon",10,"9/12/2019"); 
                    ("Lambda",0,"9/12/2019")];
      update_top_score_info ();
      assert_equal (!tuple_lst) 
        ([("Alpha",100,"9/12/2019"); ("Beta",90,"9/12/2019"); 
          ("Gamma",80,"9/12/2019"); ("Delta",70,"9/12/2019"); 
          ("Epsilon",60,"9/12/2019"); ("Alpha",50,"9/12/2019");
          ("Beta",40,"9/12/2019"); ("Gamma",30,"9/12/2019"); 
          ("Delta",20,"9/12/2019"); ("Epsilon",10,"9/12/2019")]));

  "adds current score info to list of scores" >:: (fun _ -> 
      tuple_lst := [("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
                    ("Gamma",10,"9/12/2019")];
      add_cur_score_info ("Delta",40,"9/12/2019");
      assert_equal (!tuple_lst) 
        ([("Alpha",30,"9/12/2019"); ("Beta",20,"9/12/2019"); 
          ("Gamma",10,"9/12/2019"); ("Delta",40,"9/12/2019")]));

  "producing current game over scoring info" >:: (fun _ -> 
      assert_equal (game_over_score_info go_state) 
        ("Michael Clarkson", 100, todays_date ()));

]
(************************* Settings ***************************)  
(* TEST PLAN:

   The most important feature of Settings is the ability to change features 
   of the game, namely the palette, screen size, tetro shape, and game 
   difficulty. In testing these functions, we focused on demonstrating that
   they could make the required changes. We tested that our logic for 
   modifying the settings refs worked correctly. Though we had to modify many 
   refs, the process of doing so was the same for them all, so we only tested
   one modified ref with OUnit. For the others, we printed out what the ref 
   should have been changed to when the user pressed a button, then compared
   the printed statement to the actual effect that showed up on screen. Just by
   playing the game, we could see that the colors, screen size, etc. had 
   changed, so it was easy to see that our settings functions worked. 

*)

open Gamesetup
open Settingsfuncs
open OUnit2
open Unix 

let screen_init color = Array.make_matrix !screen_height !screen_width color

let st = {
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
  game_state = Settings;
  user_name = ":)";
  hit_timer = 0.0;
  hit_counter = 0;

};;

let first_config : config = { 
  screen_height = screen_height;
  screen_width = screen_width;
  drop_time = drop_time;
  base_time = base_time;
  is_rect = is_rect;
  start_pos_x = start_pos_x;
  start_pos_y = start_pos_y;
  color_palette = color_palette; 
};;

let second_config : config = { 
  screen_height = ref !screen_height;
  screen_width = ref !screen_width;
  drop_time = ref !drop_time;
  base_time = ref !base_time;
  is_rect = ref !is_rect;
  start_pos_x =  ref !start_pos_x;
  start_pos_y = ref !start_pos_y;
  color_palette = ref !color_palette; 
};;

first_config.screen_height := 20;;
second_config.screen_height := 7;;

let settings_tests = [
  "first_config can be mutated" >:: (fun _ -> 
      assert_equal !(first_config.screen_height) 20);
  "second_config can be mutated without changing first_config" >:: (fun _ -> 
      assert_equal !(second_config.screen_height) 7);
  "second_config can be used to mutated first_config" >:: (fun _ -> 
      first_config.screen_height := !(second_config.screen_height); 
      assert_equal !(first_config.screen_height) 7);
  "clicking outside a button does nothing" >:: (fun _ -> 
      second_config.drop_time := -1.0;
      ignore (make_change (0, 0) st);
      assert_equal !(second_config.drop_time) (-1.0));
]

let tests = [
  move_down_soft_tests; 
  move_down_hard_tests;
  move_left_tests;
  move_right_tests;
  rotate_tests;
  i_tests;
  swap_tests;
  game_over_tests;
  leaderboard_tests;
  settings_tests;
  bomb_tests;
]

let suite = "search test suite" >::: List.flatten tests

let _ = run_test_tt_main suite


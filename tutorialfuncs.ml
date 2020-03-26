open Graphics
open Gamesetup
open Drawfuncs

(** [left_e] is the space in in the left edge. *) 
let left_e = 3

(**[draw_controls ()] draws the controls pane *)
let draw_controls () =
  set_color black;
  draw_rect buffer buffer 
    (!screen_width*pixel_size) (!screen_height*pixel_size);
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*5);
  draw_string ("Keys:");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*10);
  draw_string ("'A': move left 'D': move right");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*13);
  draw_string ("'S': soft drop 'W': hard drop");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*16);
  draw_string ("'E': rotate clockwise ");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*19);
  draw_string ("'Q': rotate anti-clockwise");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*25);
  draw_string ("Special blocks:");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*30);
  draw_string ("Bomb block");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*33);
  draw_string ("  Removes entire line");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*36);
  draw_string ("  it lands on");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*42);
  draw_string ("Freeze block");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*45);
  draw_string ("  Slows dropping speed");
  moveto (buffer*11) (!screen_height*pixel_size - buffer*48);
  draw_string ("  in the next turn");
  if !is_rect then 
    ( 
      draw_block (new_bomb_block (1, !screen_height - 7));
      draw_block (new_freeze_block (1, !screen_height - 10)); 
    )
  else
    (
      draw_block_round (new_bomb_block (1, !screen_height - 7));
      draw_block_round (new_freeze_block (1, !screen_height - 10));
    );
  moveto 0 0;
  set_color black;
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*54);
  draw_string ("Notes:");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*60);
  draw_string ("1 You can swap once per block");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*63);
  draw_string ("2 Clearing 3+ lines at once "); 
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*66);
  draw_string ("  gives more points");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*69);
  draw_string ("3 Difficulty increases as");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*72);
  draw_string (" the game progresses");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*75);
  draw_string ("4 You have another 0.7 sec");
  moveto (buffer*left_e) (!screen_height*pixel_size - buffer*78);
  draw_string ("  to move after landing");
  ()

(**[draw_queue ()] draws the queue pane *)
let draw_queue () = 
  draw_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  moveto (!screen_width*pixel_size+buffer*4) 
    (!screen_height*pixel_size - buffer*5);
  draw_string ("Queue area:");
  moveto (!screen_width*pixel_size+buffer*4) 
    (!screen_height*pixel_size - buffer*10);
  draw_string ("The next");
  moveto (!screen_width*pixel_size+buffer*4) 
    (!screen_height*pixel_size - buffer*13);
  draw_string ("three blocks");
  moveto (!screen_width*pixel_size+buffer*4) 
    (!screen_height*pixel_size - buffer*16);
  draw_string ("will be");
  moveto (!screen_width*pixel_size+buffer*4) 
    (!screen_height*pixel_size - buffer*19);
  draw_string ("displayed here");
  moveto 0 0

(**[draw_hold_box ()] draws the hold box pane *)
let draw_hold_box () =
  draw_rect (!screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size);
  moveto (!screen_width*pixel_size+buffer*4) 
    ((!screen_height-sidebar_h)*pixel_size - buffer*4);
  draw_string ("Hold area:");
  moveto (!screen_width*pixel_size+buffer*4) 
    ((!screen_height-sidebar_h)*pixel_size - buffer*8);
  draw_string ("Press space to");
  moveto (!screen_width*pixel_size+buffer*4) 
    ((!screen_height-sidebar_h)*pixel_size - buffer*11);
  draw_string ("hold a block");
  moveto (!screen_width*pixel_size+buffer*4) 
    ((!screen_height-sidebar_h)*pixel_size - buffer*14);
  draw_string ("for future use")

(** [draw_exit_button ()] draws the exit button *)
let draw_exit_button () = 
  draw_word (((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*19), 
             square_size*4) square_size (classic Red) (str_to_list "exit");
  ()

let draw_tutorial_screen () = 
  draw_controls();
  draw_queue();
  draw_hold_box();
  draw_exit_button()

let parse_exit_tutorial () =
  parse_button ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*19)
    ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*4)
    (square_size*4) (square_size*9)

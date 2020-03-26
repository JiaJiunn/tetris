open Graphics 
open Gamesetup
open Drawfuncs

(*********************************** TITLE ******************************)

let gap = 2
let scale = 5
let letter_width = 10*scale

(** [draw_tetris_T x y] draws T in location x y*)  
let draw_tetris_T x y =
  set_color (classic Red);
  draw_rect x (y+10*scale) (10*scale) (4*scale);
  fill_rect x (y+10*scale) (10*scale) (4*scale);
  draw_rect (x+3*scale) y (4*scale) (12*scale);
  fill_rect (x+3*scale) y (4*scale) (12*scale)

(** [draw_tetris_E x y] draws E in location x y*)
let draw_tetris_E x y = 
  set_color (classic Orange);
  draw_rect x (y+10*scale) (10*scale) (4*scale);
  fill_rect x (y+10*scale) (10*scale) (4*scale);
  draw_rect x (y+3*scale) (4*scale) (8*scale);
  fill_rect x (y+3*scale) (4*scale) (8*scale);
  draw_poly (Array.of_list [(x, y+5*scale); (x, y+8*scale); 
                            (x+9*scale, y+8*scale); (x+6*scale, y+5*scale)]);
  fill_poly (Array.of_list [(x, y+5*scale); (x, y+8*scale); 
                            (x+9*scale, y+8*scale); (x+6*scale, y+5*scale)]);
  draw_poly (Array.of_list [(x+4*scale, y); (x+4*scale, y+3*scale); 
                            (x+10*scale, y+3*scale); (x+12*scale, y)]);
  fill_poly (Array.of_list [(x+4*scale, y); (x+4*scale, y+3*scale); 
                            (x+10*scale, y+3*scale); (x+12*scale, y)]);
  draw_arc (x+4*scale) (y+4*scale) (4*scale) (4*scale) 180 270;
  fill_arc (x+4*scale) (y+4*scale) (4*scale) (4*scale) 180 270

(** [draw_tetris_T2 x y] draws the second T in location x y*)
let draw_tetris_T2 x y =
  set_color (classic Yellow);  
  draw_rect x (y+10*scale) (10*scale) (4*scale);
  fill_rect x (y+10*scale) (10*scale) (4*scale);
  draw_rect (x+3*scale) y (4*scale) (12*scale);
  fill_rect (x+3*scale) y (4*scale) (12*scale)

(** [draw_tetris_R x y] draws R in location x y *)
let draw_tetris_R x y =
  set_color (classic Green);
  draw_rect x y (4*scale) (14*scale);
  fill_rect x y (4*scale) (14*scale);
  draw_rect x (y+10*scale) (6*scale) (4*scale);
  fill_rect x (y+10*scale) (6*scale) (4*scale);
  draw_poly (Array.of_list [(x+6*scale, y+10*scale); (x+10*scale, y+10*scale); 
                            (x+8*scale, y+8*scale); (x+4*scale, y+8*scale)]);
  fill_poly (Array.of_list [(x+6*scale, y+10*scale); (x+10*scale, y+10*scale); 
                            (x+8*scale, y+8*scale); (x+4*scale, y+8*scale)]);
  draw_poly (Array.of_list [(x+8*scale, y+8*scale); (x+10*scale, y+4*scale); 
                            (x+10*scale, y); (x+4*scale, y+8*scale)]);
  fill_poly (Array.of_list [(x+8*scale, y+8*scale); (x+10*scale, y+4*scale); 
                            (x+10*scale, y); (x+4*scale, y+8*scale)]);
  draw_arc (x+6*scale) (y+10*scale) (4*scale) (4*scale) 0 90;
  fill_arc (x+6*scale) (y+10*scale) (4*scale) (4*scale) 0 90

(** [draw_tetris_I x y] draws I in location x y *)
let draw_tetris_I x y =
  set_color (classic Cyan);
  draw_rect x y (4*scale) (14*scale);
  fill_rect x y (4*scale) (14*scale)

(** [draw_tetris_S x y] draws S in location x y *)
let draw_tetris_S x y =
  set_color (classic Purple);
  draw_rect x y (6*scale) (4*scale);
  fill_rect x y (6*scale) (4*scale);
  draw_arc (x+6*scale) (y+4*scale) (4*scale) (4*scale) 270 360;
  fill_arc (x+6*scale) (y+4*scale) (4*scale) (4*scale) 270 360;
  draw_poly (Array.of_list [(x+6*scale, y+4*scale); (x, y+10*scale); 
                            (x+4*scale, y+10*scale); (x+10*scale, y+4*scale)]);
  fill_poly (Array.of_list [(x+6*scale, y+4*scale); (x, y+10*scale); 
                            (x+4*scale, y+10*scale); (x+10*scale, y+4*scale)]);
  draw_arc (x+4*scale) (y+10*scale) (4*scale) (4*scale) 90 180;
  fill_arc (x+4*scale) (y+10*scale) (4*scale) (4*scale) 90 180;
  draw_poly (Array.of_list [(x+4*scale, y+10*scale); (x+4*scale, y+14*scale); 
                          (x+10*scale, y+14*scale); (x+8*scale, y+10*scale)]);
  fill_poly (Array.of_list [(x+4*scale, y+10*scale); (x+4*scale, y+14*scale); 
                          (x+10*scale, y+14*scale); (x+8*scale, y+10*scale)])



(************************** DRAWING *************************)

(* 0: not on a button
   1: on start
   2: on tutorial
   3: on leaderboard
   4: on settings *)
let which_button = ref 0

(** [parse_start_hover ()] returns true if mouse is hovering on start button *)
let parse_start_hover () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*5*2) in
  let y = (4*pixel_size + buffer * 7) in
  parse_button_hover x (x+square_size*(5*4-1)) y (y+square_size*5)

(** [parse_tutorial_hover ()] returns true if mouse is hovering on tutorial 
button *)
let parse_tutorial_hover () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*8*2) in 
  let y = (3*pixel_size + buffer * 6) in
  parse_button_hover x (x+square_size*(8*4-1)) y (y+square_size*5)

(** [parse_leaderboard_hover ()] returns true if mouse is hovering on 
leaderboard button *)
let parse_leaderboard_hover () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*11*2) in
  let y = (2*pixel_size + buffer * 5) in
  parse_button_hover x (x+square_size*(11*4-1)) y (y+square_size*5)

(** [parse_settings_hover ()] returns true if mouse is hovering on settings
 button *)
let parse_settings_hover () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*8*2) in 
  let y = (1*pixel_size + buffer * 4) in
  parse_button_hover x (x+square_size*(8*4-1)) y (y+square_size*5)

(** [parse_hover ()] changes [which_button] according to the mouse location *)
let parse_hover () = 
  if parse_start_hover () then which_button := 1
  else if parse_tutorial_hover () then which_button := 2
  else if parse_leaderboard_hover () then which_button := 3
  else if parse_settings_hover () then which_button := 4
  else which_button := 0

(** [same_button ()] returns true if the button is true from last frame to 
this frme *)
let same_button () = 
  let old_button = !which_button in
  parse_hover ();
  (old_button == !which_button)

(** [user_col] is the color used for user_name in welcome message *)
let user_col = 
  Random.self_init (); 
  match Random.int 4 with 
  | 0 -> Blue 
  | 1 -> Orange 
  | 2 -> Green 
  | 3 -> Red
  | x -> Purple 

let draw_inactive_screen () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  draw_word ((wind_w/2-(square_size)*4*4), 
             (!screen_height/2*pixel_size + 16*scale)) square_size 
    (classic Red) (str_to_list "terminal");

  draw_tetris_T (wind_w/2 + buffer - (gap*5) - (27*scale)) 
    (!screen_height/2*pixel_size);
  draw_tetris_E (wind_w/2 + buffer - (gap*3) - (17*scale)) 
    (!screen_height/2*pixel_size);
  draw_tetris_T2 (wind_w/2 + buffer - (gap*1) - (7*scale)) 
    (!screen_height/2*pixel_size);
  draw_tetris_R (wind_w/2 + buffer + (gap*1) + (3*scale)) 
    (!screen_height/2*pixel_size);
  draw_tetris_I (wind_w/2 + buffer + (gap*3) + (13*scale)) 
    (!screen_height/2*pixel_size);
  draw_tetris_S (wind_w/2 + buffer + (gap*5) + (17*scale)) 
    (!screen_height/2*pixel_size);

  draw_word (square_size*4, (!screen_height*pixel_size -square_size*7)) 
    square_size (classic Black) (str_to_list "welcome player:");
  let user_len = String.length !user_name in
  set_color black;
  draw_rect (wind_w/2-(square_size)*(user_len+1)*2) 
    (!screen_height*pixel_size -square_size*16) 
    (square_size*(user_len*4+3)) (square_size);
  fill_rect (wind_w/2-(square_size)*(user_len+1)*2) 
    (!screen_height*pixel_size -square_size*16) 
    (square_size*(user_len*4+3)) (square_size);
  draw_word ((wind_w/2-square_size*user_len*2), 
             (!screen_height*pixel_size -square_size*14)) 
    square_size (classic user_col) (str_to_list !user_name);


  (* Active button *)
  if (parse_start_hover ()) then 
    (set_color (classic Yellow);
     draw_rect (wind_w/2-(square_size)*6*2) 
       (4*pixel_size + buffer * 7+square_size) 
       (square_size*(5*4+3)) (square_size*3);
     fill_rect (wind_w/2-(square_size)*6*2) 
       (4*pixel_size + buffer * 7+square_size) 
       (square_size*(5*4+3)) (square_size*3);
     draw_word (wind_w/2-(square_size)*5*2, 
                (4*pixel_size + buffer * 7)) square_size 
       (classic Red) ["S"; "T"; "A"; "R"; "T"])
  else
    draw_word (wind_w/2-(square_size)*5*2, (4*pixel_size + buffer * 7)) 
      square_size (classic Black) ["S"; "T"; "A"; "R"; "T"];

  (* Tutorial button *)
  if (parse_tutorial_hover ()) then 
    (set_color (classic Yellow);
     draw_rect (wind_w/2-(square_size)*9*2) 
       (3*pixel_size + buffer * 6+square_size) 
       (square_size*(8*4+3)) (square_size*3);
     fill_rect (wind_w/2-(square_size)*9*2) 
       (3*pixel_size + buffer * 6+square_size) 
       (square_size*(8*4+3)) (square_size*3);
     draw_word (wind_w/2-(square_size)*8*2, 
                (3*pixel_size + buffer * 6)) square_size 
       (classic Orange) ["T"; "U"; "T"; "O"; "R"; "I"; "A"; "L"])
  else
    draw_word (wind_w/2-(square_size)*8*2, (3*pixel_size + buffer * 6)) 
      square_size (classic Black) ["T"; "U"; "T"; "O"; "R"; "I"; "A"; "L"];

  (* Leaderbord Button *)
  if (parse_leaderboard_hover ()) then 
    (set_color (classic Yellow);
     draw_rect (wind_w/2-(square_size)*12*2) 
       (2*pixel_size + buffer * 5+square_size) 
       (square_size*(11*4+3)) (square_size*3);
     fill_rect (wind_w/2-(square_size)*12*2) 
       (2*pixel_size + buffer * 5+square_size) 
       (square_size*(11*4+3)) (square_size*3);
     draw_word (wind_w/2-(square_size)*11*2, 
                (2*pixel_size + buffer * 5)) square_size 
       (classic Blue) ["L"; "E"; "A"; "D"; "E"; "R"; "B"; "O"; "A"; "R"; "D"];
    )
  else
    draw_word (wind_w/2-(square_size)*11*2, (2*pixel_size + buffer * 5)) 
      square_size (classic Black) 
      ["L"; "E"; "A"; "D"; "E"; "R"; "B"; "O"; "A"; "R"; "D"];

  (* Settings button *)
  if (parse_settings_hover ()) then
    (set_color (classic Yellow);
     draw_rect (wind_w/2-(square_size)*9*2) 
       (1*pixel_size + buffer * 4+square_size) 
       (square_size*(8*4+3)) (square_size*3);
     fill_rect (wind_w/2-(square_size)*9*2) 
       (1*pixel_size + buffer * 4+square_size) 
       (square_size*(8*4+3)) (square_size*3);
     draw_word (wind_w/2-(square_size)*8*2, 
                (1*pixel_size + buffer * 4)) square_size 
       (classic Purple) ["S"; "E"; "T"; "T"; "I"; "N"; "G"; "S"];
    )
  else 
    draw_word (wind_w/2-(square_size)*8*2, (1*pixel_size + buffer * 4)) 
      square_size (classic Black) ["S"; "E"; "T"; "T"; "I"; "N"; "G"; "S"];
  ()


let parse_start_inactive () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*5*2) in
  let y = (4*pixel_size + buffer * 7) in
  parse_button x (x+square_size*(5*4-1)) y (y+square_size*5)

let parse_tutorial_inactive () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*8*2) in 
  let y = (3*pixel_size + buffer * 6) in
  parse_button x (x+square_size*(8*4-1)) y (y+square_size*5)

let parse_leaderboard_inactive () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*11*2) in
  let y = (2*pixel_size + buffer * 5) in
  parse_button x (x+square_size*(11*4-1)) y (y+square_size*5)

let parse_settings_inactive () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  let x = (wind_w/2-(square_size)*8*2) in 
  let y = (1*pixel_size + buffer * 4) in
  parse_button x (x+square_size*(8*4-1)) y (y+square_size*5)


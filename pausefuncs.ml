open Gamesetup 
open Graphics 
open Drawfuncs

let draw_pause_screen () = 
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in 
  draw_word (wind_w/2-square_size*11*2, !screen_height*pixel_size*3/4) 
    square_size (classic Black) (str_to_list "game paused");
  (* resume button *)
  draw_word 
    (wind_w/2-square_size*6*2, !screen_height*pixel_size*3/4-square_size*10) 
    square_size (classic Green) (str_to_list "resume");
  (* exit button *)
  draw_word 
    (wind_w/2-square_size*4*2, !screen_height*pixel_size*3/4-square_size*17) 
    square_size (classic Red) (str_to_list "exit");
  ()

let parse_resume_pause () =
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3  in
  parse_button (wind_w/2-square_size*6*2)
    (wind_w/2+square_size*11)
    (!screen_height*pixel_size*3/4-square_size*10) 
    (!screen_height*pixel_size*3/4-square_size*5)

let parse_exit_pause () =
  let wind_w = ((!screen_width + sidebar_w)*pixel_size)+buffer*3 in
  parse_button (wind_w/2-square_size*4*2)
    (wind_w/2+square_size*7)
    (!screen_height*pixel_size*3/4-square_size*17) 
    (!screen_height*pixel_size*3/4-square_size*12)


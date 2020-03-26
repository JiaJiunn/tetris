open Gamesetup
open Graphics 
open Unix
open Drawfuncs

(***************** Constants / Set-Up ******************) 

(** [screen_init color] returns a [screen] with filled with given [color]. *) 
let screen_init color = Array.make_matrix !screen_height !screen_width color

(** [settings_init_state ()] returns a initialized state for settings game 
state. *)  
let settings_init_state () = 
  {
    cur_block = new_block ();
    queue = [new_block (); new_block (); new_block ()];
    screen = screen_init White;
    num_lines_removed = 0;
    num_lines_multiple = 0;
    score = 0;
    score_multiple = 0;
    refresh = false;
    timer = gettimeofday ();
    held_block = None;
    can_swap = true;
    game_state = Inactive; 
    user_name = !user_name;
    hit_counter = 1 ; 
    hit_timer = gettimeofday ();
  }

type settings_button = 
  | DifE | DifM | DifH 
  | PalClassic | PalGrayscale | PalNight | PalSunset | PalBamboo | PalFlamingo 
  | PalCaramel | PalPastel | PalBeach | PalWatermelon 
  | SmallSize | MedSize | BigSize 
  | Round | Square 
  | Cancel | SaveChanges | Nonbutton 

type config = {
  screen_height : int ref;
  screen_width : int ref; 
  drop_time : float ref;
  base_time : float ref;
  is_rect : bool ref;
  start_pos_x : int ref;
  start_pos_y : int ref; 
  color_palette : (colors -> Graphics.color) ref
}

let main_config : config = { 
  screen_height = screen_height;
  screen_width = screen_width;
  drop_time = drop_time;
  base_time = base_time;
  is_rect = is_rect;
  start_pos_x = start_pos_x;
  start_pos_y = start_pos_y;
  color_palette = color_palette; 
};;

let temp_config = { 
  screen_height = ref !screen_height;
  screen_width = ref !screen_width;
  drop_time = ref !drop_time;
  base_time = ref !base_time;
  is_rect = ref !is_rect;
  start_pos_x =  ref !start_pos_x;
  start_pos_y = ref !start_pos_y;
  color_palette = ref !color_palette; 
};;

let window_width () = 
  (!screen_width * pixel_size + buffer + sidebar_w * pixel_size)
let window_height () = (!screen_height * pixel_size )

let cancel_w = button_w
let cancel_h = button_h
let save_w = button_w
let save_h = button_h
let dif_w = 2
let dif_h = 1
let shape_dim = 1
let size_w = 2
let size_h = 1
let pal_dim = 1

(***************** Button Functions ******************) 

(** [erase_rect] is a helper function that "erases" a drawn rectangle by
    drawing over it in white. *)
let erase_rect x y width height = 
  set_color white;
  draw_rect x y width height;
  set_color black

(** [erase_circle] is a helper function that "erases" a drawn circle by
    drawing over it in white. *)
let erase_circle x y r =
  set_color white;
  draw_circle x y r;
  set_color black

(** [find_button x y] determines which button is located at a certain
    position. *)
let find_button x y = 
  if ((square_size*5) <= x) &&
     (x <= (square_size*28))&&
     ((square_size*2) <= y) &&
     (y <= (square_size*7))
  then Cancel 

  else if ((window_width()-square_size*17) <= x) &&
          (x <= (window_width()-square_size*2))&&
          ((square_size*2) <= y) &&
          (y <= (square_size*7))
  then SaveChanges

  else if ((window_width()*3/4-square_size*4*2) <= x) && 
          (x <= (window_width()*3/4+square_size*7)) &&
          ((!screen_height/2*pixel_size-square_size*22) <= y) && 
          (y <= (!screen_height/2*pixel_size-square_size*17))
  then DifE

  else if ((window_width()*3/4-square_size*6*2) <= x) && 
          (x <= (window_width()*3/4+square_size*3)) &&
          ((!screen_height/2*pixel_size-square_size*29) <= y) && 
          (y <= (!screen_height/2*pixel_size-square_size*24))
  then DifM

  else if ((window_width()*3/4-square_size*4*2) <= x) && 
          (x <= (window_width()*3/4+square_size*7)) &&
          ((!screen_height/2*pixel_size-square_size*36) <= y) && 
          (y <= (!screen_height/2*pixel_size-square_size*31))
  then DifH

  else if (((window_width()/4)-(shape_dim*pixel_size/2)) <= x) &&
          (x <= ((window_width()/4)+(shape_dim*pixel_size/2))) &&
          ((!screen_height/2*pixel_size-square_size*25) <= y) &&
          (y <= ((!screen_height/2*pixel_size-square_size*25) 
                 + shape_dim* pixel_size))
  then Square

  else if (((window_width()/4)-(shape_dim*pixel_size/2)) <= x) &&
          (x <= ((window_width()/4)+(shape_dim*pixel_size/2))) &&
          ((!screen_height/2*pixel_size-square_size*32
            -(shape_dim*pixel_size/2))<= y) &&
          (y <= ((!screen_height/2*pixel_size-square_size*32) 
                 + shape_dim* pixel_size))
  then Round

  else if ((square_size*6)<= x) &&
          (x <= (square_size*25)) &&
          ((!screen_height/2*pixel_size-square_size
            -(shape_dim*pixel_size/2)) <= y) &&
          (y <= (!screen_height/2*pixel_size+square_size*4+
                 (shape_dim*pixel_size/2)))
  then SmallSize

  else if ((window_width()/2-square_size*5*2) <= x) &&
          (x <= (window_width()/2+square_size*9)) &&
          ((!screen_height/2*pixel_size-square_size) <= y) &&
          (y <= (!screen_height/2*pixel_size+square_size*4))
  then MedSize

  else if ((window_width()-square_size*22) <= x) &&
          (x <= (window_width()-square_size*3)) &&
          ((!screen_height/2*pixel_size-square_size) <= y) &&
          (y <= (!screen_height/2*pixel_size+square_size*4))
  then BigSize

  else if (3 * (window_width()) / 18 - buffer <= x) &&
          (x <= 3 * (window_width()) / 18 + shape_dim * pixel_size - buffer) && 
          (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size <= y) &&
          (y <= !screen_height*pixel_size-square_size*16) 
  then PalClassic

  else if (6 * (window_width()) / 18 - buffer <= x) &&
          (x <= 6 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size <= y) &&
          (y <= !screen_height*pixel_size-square_size*16) 
  then PalGrayscale

  else if (9 * (window_width()) / 18 - buffer <= x) &&
          (x <= 9 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size <= y) &&
          (y <= !screen_height*pixel_size-square_size*16) 
  then PalNight

  else if (12 * (window_width()) / 18 - buffer <= x) &&
          (x <= 12 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size <= y) &&
          (y <= !screen_height*pixel_size-square_size*16) 
  then PalSunset

  else if (15 * (window_width()) / 18 - buffer <= x) &&
          (x <= 15 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size <= y) &&
          (y <= !screen_height*pixel_size-square_size*16) 
  then PalBamboo

  else if (3 * (window_width()) / 18 - buffer <= x) &&
          (x <= 3 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*24
           -pal_dim*pixel_size*2 <= y) &&
          (y <= !screen_height*pixel_size-square_size*18-pal_dim*pixel_size) 
  then PalFlamingo

  else if (6 * (window_width()) / 18 - buffer <= x) &&
          (x <= 6 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*24
           -pal_dim*pixel_size*2 <= y) &&
          (y <= !screen_height*pixel_size-square_size*18-pal_dim*pixel_size) 
  then PalCaramel

  else if (9 * (window_width()) / 18 - buffer <= x) &&
          (x <= 9 * (window_width()) / 18 - buffer + shape_dim * pixel_size) && 
          (!screen_height*pixel_size-square_size*24
           -pal_dim*pixel_size*2 <= y) &&
          (y <= !screen_height*pixel_size-square_size*18-pal_dim*pixel_size) 
  then PalPastel

  else if (12 * (window_width()) / 18 - buffer <= x) &&
          (x <= 12 * (window_width()) / 18 - buffer + shape_dim * pixel_size)&& 
          (!screen_height*pixel_size-square_size*24
           -pal_dim*pixel_size*2 <= y) &&
          (y <= !screen_height*pixel_size-square_size*18-pal_dim*pixel_size) 
  then PalBeach

  else if (15 * (window_width()) / 18 - buffer <= x) &&
          (x <= 15 * (window_width()) / 18 - buffer + shape_dim * pixel_size)&& 
          (!screen_height*pixel_size-square_size*24
           -pal_dim*pixel_size*2 <= y) &&
          (y <= !screen_height*pixel_size-square_size*18-pal_dim*pixel_size) 
  then PalWatermelon

  else Nonbutton

let make_change (x, y) state = 
  match find_button x y with 
  | DifE -> 
    temp_config.drop_time := 2.5;
    temp_config.base_time := !(temp_config.drop_time);
    {state with refresh = true}
  | DifM -> 
    temp_config.drop_time := 2.0;
    temp_config.base_time := !(temp_config.drop_time);
    {state with refresh = true}
  | DifH -> 
    temp_config.drop_time := 1.5;
    temp_config.base_time := !(temp_config.drop_time);
    {state with refresh = true}
  | Square -> 
    temp_config.is_rect := true;
    {state with refresh = true}
  | Round -> 
    temp_config.is_rect := false;
    {state with refresh = true}
  | SmallSize ->
    temp_config.screen_width := 10;
    temp_config.screen_height := 20;
    temp_config.start_pos_x := 5;
    temp_config.start_pos_y := 20;
    {state with refresh = true}
  | MedSize ->
    temp_config.screen_width := 11;
    temp_config.screen_height := 22;
    temp_config.start_pos_x := !(temp_config.screen_width)/2;
    temp_config.start_pos_y := !(temp_config.screen_height);

    {state with refresh = true}
  | BigSize ->
    temp_config.screen_width := 12;
    temp_config.screen_height := 24;
    temp_config.start_pos_x := !(temp_config.screen_width)/2;
    temp_config.start_pos_y := !(temp_config.screen_height);

    {state with refresh = true}
  | PalClassic -> 
    temp_config.color_palette := classic;
    draw_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalGrayscale -> 
    temp_config.color_palette := grayscale;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalNight -> 
    temp_config.color_palette := night_mode;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalSunset -> 
    temp_config.color_palette := sunset;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) (
      !screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalBamboo -> 
    temp_config.color_palette := bamboo;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalFlamingo -> 
    temp_config.color_palette := flamingo;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalCaramel -> 
    temp_config.color_palette := caramel;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalPastel -> 
    temp_config.color_palette := pastel;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalBeach -> 
    temp_config.color_palette := beach;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | PalWatermelon -> 
    temp_config.color_palette := watermelon;
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    draw_rect (15 * (window_width()) / 18 - 2 * buffer) 
      (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
      (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
    {state with refresh = true}
  | Cancel -> 
    temp_config.screen_height := !(main_config.screen_height); 
    temp_config.screen_width := !(main_config.screen_width); 
    temp_config.drop_time := !(main_config.drop_time); 
    temp_config.base_time := !(main_config.base_time); 
    temp_config.is_rect := !(main_config.is_rect); 
    temp_config.start_pos_x := !(main_config.start_pos_x); 
    temp_config.start_pos_y := !(main_config.start_pos_y); 
    temp_config.color_palette := !(main_config.color_palette); 
    {state with game_state = Inactive; refresh = true} 
  | SaveChanges -> 
    main_config.screen_height := !(temp_config.screen_height); 
    main_config.screen_width := !(temp_config.screen_width); 
    main_config.drop_time := !(temp_config.drop_time); 
    main_config.base_time := !(temp_config.base_time); 
    main_config.is_rect := !(temp_config.is_rect); 
    main_config.start_pos_x := !(temp_config.start_pos_x); 
    main_config.start_pos_y := !(temp_config.start_pos_y); 
    main_config.color_palette := !(temp_config.color_palette); 
    screen_width := !(temp_config.screen_width);
    screen_height := !(temp_config.screen_height);
    start_pos_x := !(temp_config.start_pos_x);
    start_pos_y := !(temp_config.start_pos_y);
    resize_window (!(main_config.screen_width)*pixel_size + (buffer*3) 
                   + (sidebar_w*pixel_size) )
      (!(main_config.screen_height)*pixel_size + (buffer *2));
    {(settings_init_state ()) with refresh = true}
  | Nonbutton -> state

(***************** Draw Buttons and Labels ******************) 

(** [draw_settings_frame] is a helper function that draws the frame for the 
    settings menu. *)
let draw_settings_frame () =
  draw_rect buffer buffer (window_width()) (window_height())

(** [draw_labels] is a helper function that draws the lavels in the settings
    menu. *)
let draw_labels () = 
  draw_word (window_width()*3/4-square_size*10*2, 
             (!screen_height/2*pixel_size-square_size*15)) square_size 
    (classic Black) (str_to_list "difficulty");
  draw_word (window_width()/4-square_size*5*2, 
             (!screen_height/2*pixel_size-square_size*15)) square_size 
    (classic Black) (str_to_list "shape");
  draw_word (window_width()/2-square_size*11*2, 
             (!screen_height/2*pixel_size+square_size*7)) square_size 
    (classic Black) (str_to_list "screen size");
  draw_word (window_width()/2-square_size*8*2, 
             (!screen_height*pixel_size)-(square_size*15)) square_size 
    (classic Black) (str_to_list "palettes");
  draw_word (square_size*4, (!screen_height*pixel_size)-(square_size*7)) 
    square_size (classic Purple) (str_to_list "settings:")

(** [draw_cancel] is a helper function that draws the cancel button. *)
let draw_cancel () =
  draw_word (square_size*5, square_size*3) 
    square_size (classic Red) (str_to_list "cancel")

(** [draw_save] is a helper function that draws the save button. *)
let draw_save () =
  draw_word (window_width()-square_size*17, square_size*3) 
    square_size (classic Green) (str_to_list "save")

(** [draw_difE] is a helper function that draws the easy difficulty option. *)
let draw_difE () =
  draw_word (window_width()*3/4-square_size*4*2, 
             (!screen_height/2*pixel_size-square_size*22)) square_size 
    (classic Black) (str_to_list "easy")

(** [draw_difM] is a helper function that draws the medium difficulty option. *)
let draw_difM () =
  draw_word (window_width()*3/4-square_size*6*2, 
             (!screen_height/2*pixel_size-square_size*29)) square_size 
    (classic Black) (str_to_list "medium")

(** [draw_difH] is a helper function that draws the hard difficulty option. *)
let draw_difH () =
  draw_word (window_width()*3/4-square_size*4*2, 
             (!screen_height/2*pixel_size-square_size*36)) square_size 
    (classic Black) (str_to_list "hard")

(** [draw_square_tetro] is a helper function that draws the 
    square tetris piece option. *)
let draw_square_tetro () = 
  draw_rect ((window_width()/4)-(shape_dim*pixel_size/2)) 
    (!screen_height/2*pixel_size-square_size*25)
    (shape_dim * pixel_size) (shape_dim * pixel_size)

(** [draw_round_tetro] is a helper function that draws the 
    round tetris piece option. *)
let draw_round_tetro () = 
  draw_circle (window_width()/4) (!screen_height/2*pixel_size-square_size*32)
    (shape_dim * pixel_size / 2)

(** [draw_small_size] is a helper function that draws the 
    small size option. *)
let draw_small_size () = 
  draw_word ((square_size*6), (!screen_height/2*pixel_size-square_size)) 
    square_size (classic Black) (str_to_list "10*20")

(** [draw_med_size] is a helper function that draws the 
    medium size option. *)
let draw_med_size () =   
  draw_word ((window_width()/2-square_size*8), 
             (!screen_height/2*pixel_size-square_size)) square_size 
    (classic Black) (str_to_list "11*22")

(** [draw_big_size] is a helper function that draws the 
    big size option. *)
let draw_big_size () =
  draw_word ((window_width()-square_size*22), 
             (!screen_height/2*pixel_size-square_size)) square_size 
    (classic Black) (str_to_list "12*24")


(** [draw_palette_colors col1 col2 col3 x y w h] draws a square with 
    width [w] and height [w] at position [x,y] made up of three 
    horizontally-layered colors. The bottom color is determined by [col1],
    the middle by [col2], and the top by [col3]. *)
let draw_palette_colors col1 col2 col3 x y w h =
  set_color col1; 
  fill_rect x y w (h/3);
  set_color col2;
  fill_rect x (y + h/3) w (h / 3);
  set_color col3;
  fill_rect x (y + (2 * h / 3)) w (h / 3);
  set_color black

(** [draw_pal_classic] is a helper function that draws the 
    classic palette. *)
let draw_pal_classic () =
  draw_palette_colors (classic Blue) (classic Yellow) (classic Red)
    (3 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (3 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_grayscale] is a helper function that draws the 
    grayscale palette. *)
let draw_pal_grayscale () =
  draw_palette_colors (grayscale Blue) (grayscale Cyan) (grayscale Freeze)
    (6 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (6 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_night_mode] is a helper function that draws the 
    night mode palette. *)
let draw_pal_night_mode () =
  draw_palette_colors (night_mode Purple) (night_mode Green) (night_mode Blue)
    (9 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (9 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_sunset] is a helper function that draws the 
    sunset palette. *)
let draw_pal_sunset () =
  draw_palette_colors (sunset Red) (sunset Yellow) (sunset Purple) 
    (12 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (12 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_bamboo] is a helper function that draws the 
    bamboo palette. *)
let draw_pal_bamboo () =
  draw_palette_colors (bamboo Cyan) (bamboo Yellow) (bamboo White) 
    (15 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (15 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_flamingo] is a helper function that draws the 
    flamingo palette. *)
let draw_pal_flamingo () =
  draw_palette_colors (flamingo Green) (flamingo Yellow) (flamingo White) 
    (3 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (3 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_caramel] is a helper function that draws the 
    caramel palette. *)
let draw_pal_caramel () =
  draw_palette_colors (caramel Green) (caramel Orange) (caramel Red) 
    (6 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (6 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_pastel] is a helper function that draws the 
    pastel palette. *)
let draw_pal_pastel () =
  draw_palette_colors (pastel Blue) (pastel Yellow) (pastel Red) 
    (9 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (9 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_beach] is a helper function that draws the 
    beach palette. *)
let draw_pal_beach () =
  draw_palette_colors (beach White) (beach Yellow) (beach Blue)
    (12 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (12 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(** [draw_pal_watermelon] is a helper function that draws the 
    watermelon palette. *)
let draw_pal_watermelon () =
  draw_palette_colors (watermelon Orange) (watermelon Red) (watermelon Green)
    (15 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size);
  draw_rect (15 * (window_width()) / 18 - buffer) 
    (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2+buffer)
    (pal_dim * pixel_size) (pal_dim * pixel_size)

(***************** Draw Selections ******************) 

(** [draw_palette_selection config] is a helper function that draws the 
    palette selection. *)
let draw_palette_selection (c : config) = 
  let pal = !(c.color_palette) in
  if pal == classic then
    (draw_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer))
  else if pal == grayscale then 
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (6 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 

       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer))
  else if pal == night_mode then 
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer))
  else if pal == sunset then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer)
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);)
  else if pal == bamboo then 
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);)
  else if pal == flamingo then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
       )
  else if pal == caramel then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
       )
  else if pal == pastel then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);)
  else if pal == beach then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
       )
  else if pal == watermelon then
    (erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*20-pal_dim*pixel_size)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (3 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (6 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (9 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     erase_rect (12 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
     draw_rect (15 * (window_width()) / 18 - 2 * buffer) 
       (!screen_height*pixel_size-square_size*24-pal_dim*pixel_size*2)
       (pal_dim * pixel_size + 2 * buffer) (pal_dim * pixel_size + 2 * buffer);
       )

(** [draw_shape_selection config] is a helper function that draws the 
    shape selection. *)
let draw_shape_selection (c : config) = 
  match !(c.is_rect) with 
  | true -> 
    draw_rect ((window_width()/4)-(shape_dim*pixel_size/2)-buffer) 
      (!screen_height/2*pixel_size-square_size*25-buffer)
      (shape_dim * pixel_size + 2*buffer) (shape_dim * pixel_size + 2*buffer);
    erase_circle (window_width()/4) 
    (!screen_height/2*pixel_size-square_size*32) 
      (shape_dim * pixel_size / 2 + buffer)
  | false -> 
    erase_rect ((window_width()/4)-(shape_dim*pixel_size/2)-buffer) 
      (!screen_height/2*pixel_size-square_size*25-buffer)
      (shape_dim * pixel_size + 2*buffer) (shape_dim * pixel_size + 2*buffer);
    draw_circle (window_width()/4) 
    (!screen_height/2*pixel_size-square_size*32) 
      (shape_dim * pixel_size / 2 + buffer)

(** [draw_size_selection config] is a helper function that draws the 
    size selection. *)
let draw_size_selection (c : config) = 
  match !(c.screen_height) with 
  | 20 ->
    set_color (classic Yellow);
    fill_rect (square_size*4) (!screen_height/2*pixel_size) 
      (square_size*23) (square_size*3);
    draw_word ((square_size*6), (!screen_height/2*pixel_size-square_size)) 
      square_size (classic Black) (str_to_list "10*20");
    draw_word ((window_width()/2-square_size*8), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "11*22");
    draw_word ((window_width()-square_size*22), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "12*24")

  | 22 ->
    draw_word ((square_size*6), (!screen_height/2*pixel_size-square_size)) 
      square_size (classic Black) (str_to_list "10*20");
    set_color (classic Yellow);
    fill_rect (window_width()/2-square_size*10) (!screen_height/2*pixel_size) 
      (square_size*23) (square_size*3);
    draw_word ((window_width()/2-square_size*8), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "11*22");
    draw_word ((window_width()-square_size*22), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "12*24")

  | 24 -> 
    draw_word ((square_size*6), (!screen_height/2*pixel_size-square_size)) 
      square_size (classic Black) (str_to_list "10*20");
    draw_word ((window_width()/2-square_size*8), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "11*22");
    set_color (classic Yellow);
    fill_rect (window_width()-square_size*24) (!screen_height/2*pixel_size) 
      (square_size*23) (square_size*3);
    draw_word ((window_width()-square_size*22), 
               (!screen_height/2*pixel_size-square_size)) square_size 
      (classic Black) (str_to_list "12*24")


  | _ -> failwith "Invalid screen size."

(** [draw_dif_selection config] is a helper function that draws the 
    difficulty selection. *)
let draw_dif_selection (c : config) = 
  match !(c.drop_time) with 
  | 2.5 ->
    set_color (classic Yellow);
    fill_rect (window_width()*3/4-square_size*10) 
      (!screen_height/2*pixel_size-square_size*21) 
      (square_size*19) (square_size*3);
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*22)) square_size 
      (classic Black) (str_to_list "easy");
    draw_word (window_width()*3/4-square_size*6*2, 
               (!screen_height/2*pixel_size-square_size*29)) square_size 
      (classic Black) (str_to_list "medium");
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*36)) square_size 
      (classic Black) (str_to_list "hard")

  | 2.0 -> 
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*22)) square_size 
      (classic Black) (str_to_list "easy");
    set_color (classic Yellow);
    fill_rect (window_width()*3/4-square_size*14) 
      (!screen_height/2*pixel_size-square_size*28) 
      (square_size*23) (square_size*3);
    draw_word (window_width()*3/4-square_size*6*2, 
               (!screen_height/2*pixel_size-square_size*29)) square_size 
      (classic Black) (str_to_list "medium");
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*36)) square_size 
      (classic Black) (str_to_list "hard")

  | 1.5 ->
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*22)) square_size 
      (classic Black) (str_to_list "easy");
    draw_word (window_width()*3/4-square_size*6*2, 
               (!screen_height/2*pixel_size-square_size*29)) square_size 
      (classic Black) (str_to_list "medium");
    set_color (classic Yellow);
    fill_rect (window_width()*3/4-square_size*10) 
      (!screen_height/2*pixel_size-square_size*35) 
      (square_size*19) (square_size*3);
    draw_word (window_width()*3/4-square_size*4*2, 
               (!screen_height/2*pixel_size-square_size*36)) square_size 
      (classic Black) (str_to_list "hard")

  | _ -> failwith "Invalid difficulty."

let draw_settings_screen (s : Gamesetup.state) : unit = 
  draw_settings_frame ();
  draw_labels ();
  draw_cancel ();
  draw_save ();
  draw_difE ();
  draw_difM ();
  draw_difH ();
  draw_square_tetro ();
  draw_round_tetro ();
  draw_small_size ();
  draw_med_size ();
  draw_big_size ();
  draw_pal_classic ();
  draw_pal_grayscale ();
  draw_pal_night_mode ();
  draw_pal_sunset ();
  draw_pal_bamboo ();
  draw_pal_flamingo ();
  draw_pal_caramel ();
  draw_pal_pastel ();
  draw_pal_beach ();
  draw_pal_watermelon ();
  draw_palette_selection temp_config;
  draw_shape_selection temp_config;
  draw_size_selection temp_config;
  draw_dif_selection temp_config

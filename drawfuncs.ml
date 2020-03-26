open Gamesetup 
open Graphics
open Movetetro

(************************ CHARACTERS DRAWING ************************)
let square_size = 4

(** [draw_square (x,y)] draws a square at location [(x,y)]. *)
let draw_square (x, y) s = 
  draw_rect x y s s;
  fill_rect x y s s

let rec str_to_list str = match str with
  | "" -> []
  | str -> (Char.escaped (String.get str 0 )) :: 
           (str_to_list (String.sub str 1 ( (String.length str)-1) ) )

(** [letter ch] is the list of coordinates to draw the character [ch]. *)
let letter ch = 
  match ch with 
  | "A" | "a" -> [(0, 0); (0, 1); (0, 2); (0, 3); (1, 2); (1, 4); (2, 0); 
                  (2, 1); (2, 2); (2, 3)]
  | "B" | "b" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); 
                  (1, 4); (2, 1); (2, 3)]
  | "C" | "c" -> [(0, 1); (0, 2); (0, 3); (1, 0); (1, 4); (2, 0); (2, 4)]
  | "D" | "d" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 4); 
                  (2, 1); (2, 2); (2, 3)]
  | "E" | "e" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); 
                  (1, 4); (2, 0); (2, 2); (2, 4)]
  | "F" | "f" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 2); (1, 4); 
                  (2, 2); (2, 4)]
  | "G" | "g" -> [(0, 1); (0, 2); (0, 3); (1, 0); (1, 2); (1, 4); (2, 0); 
                  (2, 1); (2, 2); (2, 4)]
  | "H" | "h" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 2); (2, 0); 
                  (2, 1); (2, 2); (2, 3); (2, 4)]
  | "I" | "i"-> [(0, 0); (0, 4); (1, 0); (1, 1); (1, 2); (1, 3); (1, 4); 
                 (2, 0); (2, 4)]
  | "J" | "j" -> [(0, 0); (0, 1); (0, 4); (1, 0); (1, 4); (2, 0); (2, 1); 
                  (2, 2); (2, 3); (2, 4)]
  | "K" | "k" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 1); (1, 3); 
                  (2, 0); (2, 4)]
  | "L" | "l" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (2, 0)]
  | "M" | "m" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 3); (2, 0); 
                  (2, 1); (2, 2); (2, 3); (2, 4)]
  | "N" | "n" -> [(0, 0); (0, 1); (0, 2); (0, 3);  (1, 4); (2, 0); (2, 1); 
                  (2, 2); (2, 3)]
  | "O" | "o" -> [(0, 1); (0, 2); (0, 3); (1, 0); (1, 4); (2, 1); (2, 2); 
                  (2, 3)]
  | "P" | "p" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 2); (1, 4); 
                  (2, 3)]
  | "Q" | "q" -> [(0, 2); (0, 3); (1, 1); (1, 4); (2, 0); (2, 2); (2, 3)]
  | "R" | "r" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 2); (1, 4); 
                  (2, 1); (2, 0); (2, 3)]
  | "S" | "s" -> [(0, 0); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); (1, 4); 
                  (2, 0); (2, 1); (2, 2); (2, 4)]
  | "T" | "t" -> [(0, 4); (1, 0); (1, 1); (1, 2); (1, 3); (1, 4); (2, 4)]
  | "U" | "u" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (2, 0); 
                  (2, 1); (2, 2); (2, 3); (2, 4)]
  | "V" | "v" -> [(0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (2, 1); (2, 2); 
                  (2, 3); (2, 4)]
  | "W" | "w" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 1); (2, 0); 
                  (2, 1); (2, 2); (2, 3); (2, 4)]
  | "X" | "x" -> [(0, 0); (0, 1); (0, 3); (0, 4); (1, 2); (2, 0); (2, 1); 
                  (2, 3); (2, 4)]
  | "Y" | "y" -> [(0, 2); (0, 3); (0, 4); (1, 0); (1, 1); (2, 2); (2, 3); 
                  (2, 4)]
  | "Z" | "z" -> [(0, 0); (0, 1); (0, 4); (1, 0); (1, 2); (1, 4); (2, 0); 
                  (2, 3); (2, 4)]
  | "0" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 4); (2, 0); 
            (2, 1); (2, 2); (2, 3); (2, 4)]
  | "1" -> [(0, 4); (1, 0); (1, 1); (1, 2); (1, 3); (1, 4)]
  | "2" -> [(0, 0); (0, 1); (0, 2); (0, 4); (1, 0); (1, 2); (1, 4); 
            (2, 0); (2, 2); (2, 3); (2, 4)]
  | "3" -> [(0, 0); (0, 2); (0, 4); (1, 0); (1, 2); (1, 4); (2, 0); 
            (2, 1); (2, 2); (2, 3); (2, 4)]
  | "4" -> [(0, 2); (0, 3); (0, 4); (1, 2); (2, 0); (2, 1); (2, 2); 
            (2, 3); (2, 4)]
  | "5" -> [(0, 0); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); (1, 4); 
            (2, 0); (2, 1); (2, 2); (2, 4)]
  | "6" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); 
            (1, 4); (2, 0); (2, 1); (2, 2); (2, 4)]
  | "7" -> [(0, 3); (0, 4); (1, 4); (2, 0); (2, 1); (2, 2); (2, 3); 
            (2, 4)]
  | "8" -> [(0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); 
            (1, 4); (2, 0); (2, 1); (2, 2); (2, 3); (2, 4)]
  | "9" -> [(0, 0); (0, 2); (0, 3); (0, 4); (1, 0); (1, 2); (1, 4); 
            (2, 0); (2, 1); (2, 2); (2, 3); (2, 4)]
  | ":" -> [(1, 1); (1, 3)]
  | "*" -> [(0, 1); (0, 3); (1, 2); (2, 1); (2, 3)]
  | _ -> []

(** [draw_letter_helper (x0, y0) s lst col)] is the lists the coordinates of
    the letters in list [lst]. *)
let draw_letter_helper (x0, y0) s lst col = 
  set_color (col); 
  List.map (function (x, y) -> (x0+x*s, y0+y*s)) lst

(** [draw_letter (x0, y0) s lst] draws the letter in list [lst]. *)
let rec draw_letter (x0, y0) s lst = 
  match lst with 
  | [] -> ()
  | h::t -> draw_square h s; draw_letter (x0, y0) s t

let rec draw_word (x0, y0) s col letters = 
  match letters with 
  | [] -> ()
  | h::t -> 
    draw_letter (x0, y0) s (draw_letter_helper (x0, y0) s (letter h) col); 
    draw_word (x0+s*4, y0) s col t

(************************ BASIC FUNCS ************************)

let draw_pixel_frame (x, y) = 
  draw_rect (x*pixel_size + buffer) (y*pixel_size + buffer) 
    pixel_size pixel_size

let draw_pixel_filled (x, y) = 
  draw_rect (x*pixel_size + piece_border + buffer) 
    (y*pixel_size + piece_border + buffer) 
    (pixel_size - 2*piece_border) 
    (pixel_size - 2*piece_border);
  fill_rect (x*pixel_size + piece_border + buffer) (
    y*pixel_size + piece_border + buffer) 
    (pixel_size - 2*piece_border) 
    (pixel_size - 2*piece_border)

(** [draw_pixel (x, y)] draws the given pixel at the specified coordinates *)
let draw_pixel (x, y) = 
  draw_pixel_frame (x, y);
  draw_pixel_filled (x, y)

let draw_block (block:block) = 
  set_color (( !color_palette) (block.color));  
  ignore(List.map (fun (x,y) -> draw_pixel (x, y)) (block_pixels block))

(** [draw_ghost_frame] draws the frame for a ghost piece, which is slightly 
    smaller than a regular frame to distinguish *)
let draw_ghost_frame (x, y) = 
  draw_rect (x*pixel_size + buffer+piece_border) 
    (y*pixel_size + buffer+piece_border) 
    (pixel_size-piece_border*2) 
    (pixel_size-piece_border*2) 

let draw_ghost_piece (ghost : block) = 
  set_color (( !color_palette) (ghost.color));
  ignore(List.map (fun (x,y) -> draw_ghost_frame (x, y)) (block_pixels ghost))

(*************** FUNCS THAT DRAW WITH ABSOLUTE COORDINATES ***************)

(** [draw_pixel_frame_abs (x,y)] draws the pixel frames at location [(x,y)] in 
    absolute coordinates. *)
let draw_pixel_frame_abs (x, y) = 
  draw_rect x y pixel_size pixel_size

(** [draw_pixel_filled_abs (x,y)] draws the filled pixels at location [(x,y)]in 
    absolute coordinates. *)
let draw_pixel_filled_abs (x, y) = 
  draw_rect (x + piece_border) (y + piece_border) 
    (pixel_size - 2*piece_border) 
    (pixel_size - 2*piece_border);
  fill_rect (x + piece_border) (y + piece_border) 
    (pixel_size - 2*piece_border) 
    (pixel_size - 2*piece_border)

(** [draw_pixel_abs (x,y)] draws the pixels at location [(x,y)] in 
    absolute coordinates. *)
let draw_pixel_abs (x, y) = 
  draw_pixel_frame_abs (x, y);
  draw_pixel_filled_abs (x, y)

(** [draw_block_abs (x,y)] draws the blocks at location [(x,y)] in 
    absolute coordinates. *)
let draw_block_abs (block:block) pixels = 
  set_color (( !color_palette) (block.color));  
  ignore(List.map (fun (x,y) -> draw_pixel_abs (x, y)) pixels)

(** [first_queue_fy block] is the list of coordinates for the block [block] 
    which is first in queue. *)
let first_queue_fy block = 
  match block.color with 
  | Cyan -> let (x0, y0) = 
              (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
               14*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size)in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Yellow -> let (x0, y0) = 
                (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                 15*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) 
    in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Green | Red ->  let (x0, y0) = 
                      (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                       15*sidebar_pix+2*buffer+
                       (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Black | Freeze ->  let (x0, y0) = 
                         (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                          14*sidebar_pix+2*buffer+
                          (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | _ -> let (x0, y0) = 
           (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
            13*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 

(** [snd_queue_fy block] is the list of coordinates for the block [block] 
    which is second in queue. *)
let snd_queue_fy block =   
  match block.color with 
  | Cyan -> let (x0, y0) = 
              (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
               8*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Yellow -> let (x0, y0) = 
                (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                 9*sidebar_pix+2*buffer+
                 (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Green | Red ->  let (x0, y0) = 
                      (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                       9*sidebar_pix+2*buffer+
                       (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Black | Freeze ->  let (x0, y0) = 
                         (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                          8*sidebar_pix+2*buffer+
                          ( !screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | _ -> let (x0, y0) = 
           (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
            7*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 

(** [thrd_queue_fy block] is the list of coordinates for the block [block] 
    which is third in queue. *)
let thrd_queue_fy block = 
  match block.color with 
  | Cyan -> let (x0, y0) = 
              (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
               2*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Yellow -> let (x0, y0) = 
                (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                 3*sidebar_pix+2*buffer+
                 (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Green | Red ->  let (x0, y0) = 
                      (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                       3*sidebar_pix+2*buffer+
                       (!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | Black | Freeze ->  let (x0, y0) = 
                         (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                          2*sidebar_pix+2*buffer+
                          ( !screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 
  | _ -> let (x0, y0) = 
           (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
            1*sidebar_pix+2*buffer+(!screen_height-sidebar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) block.pixels 

(** [draw_first queue] draws the first block in queue [queue]. *)
let draw_first queue = 
  draw_block_abs (List.hd queue) (first_queue_fy (List.hd queue))

(** [draw_second queue] draws the second block in queue [queue]. *)
let draw_second queue = 
  draw_block_abs (List.nth queue 1) (snd_queue_fy (List.nth queue 1))

(** [draw_third queue] draws the third block in queue [queue]. *)
let draw_third queue =
  draw_block_abs (List.nth queue 2) (thrd_queue_fy (List.nth queue 2))

(** [draw_queue queue] draws the three blocks in queue [queue]. *)
let draw_queue (queue : block list) =
  set_color (( !color_palette) White);
  draw_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  fill_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  draw_first queue;
  draw_second queue;
  draw_third queue

(** [draw_held_piece_helper piece] is the list of coordinates for held piece 
    [piece]. *)
let draw_held_piece_helper piece = 
  match piece.color with 
  | Cyan -> let (x0, y0) = 
              (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
               2*sidebar_pix+2*buffer +
               (!screen_height-sidebar_h-holdbar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) piece.pixels 
  | Yellow -> let (x0, y0) = 
                (3*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                 3*sidebar_pix+2*buffer +
                 (!screen_height-sidebar_h-holdbar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) piece.pixels 
  | Green | Red ->  let (x0, y0) = 
                      (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                       3*sidebar_pix+2*buffer + 
                       (!screen_height-sidebar_h-holdbar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) piece.pixels 
  | Black | Freeze ->  let (x0, y0) = 
                         (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
                          2*sidebar_pix+2*buffer+
                          ( !screen_height-sidebar_h-holdbar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) piece.pixels 
  | _ -> let (x0, y0) = 
           (4*sidebar_pix+2*buffer+ !screen_width*pixel_size, 
            1*sidebar_pix+2*buffer+
            (!screen_height-sidebar_h-holdbar_h)*pixel_size) in 
    List.map (function (x, y) -> 
        (x0+(x*pixel_size), y0+(y*pixel_size))) piece.pixels 

let draw_held_piece (piece : block option) = 
  set_color (( !color_palette) White);
  draw_rect (! screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size);
  fill_rect (!screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size);
  match piece with
  | None -> ()
  | Some block -> draw_block_abs block (draw_held_piece_helper block)

let draw_score (score : int) =
  draw_word (!screen_width*pixel_size+square_size*3, 
             (!screen_height-sidebar_h-holdbar_h)*pixel_size-square_size*7) 
    square_size black (str_to_list "score:");
  draw_word (!screen_width*pixel_size+square_size*3, 
             (!screen_height-sidebar_h-holdbar_h)*pixel_size-square_size*15) 
    square_size (classic Blue) (str_to_list (string_of_int score))

let draw_screen screen = 
  set_color (( !color_palette) White);
  draw_rect buffer buffer 
    (!screen_width*pixel_size) (!screen_height*pixel_size);
  fill_rect buffer buffer 
    (!screen_width*pixel_size) (!screen_height*pixel_size);
  for x = 0 to (!screen_width - 1) do 
    for y = 0 to (!screen_height - 1) do 
      let (x', y') = block_to_screen_pos (x, y) in 
      let color = screen.(x').(y') in
      set_color (( !color_palette) (color));
      if color != White then
        draw_pixel (x, y) else ()
    done 
  done

(************************ BORDERS ************************)
let draw_borders state = 
  set_color black;
  draw_rect buffer buffer (!screen_width*pixel_size) 
    (!screen_height*pixel_size);
  draw_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  draw_rect (!screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size)

(************************ ROUND BOIS ************************)

(** [draw_pixel_frame_round (x, y)] draws each pixel frame for round block at 
location [(x, y)]. *)
let draw_pixel_frame_round (x, y) = 
  draw_circle (x*pixel_size + buffer + pixel_size/2) 
    (y*pixel_size + buffer + pixel_size/2) (pixel_size/2)

(** [draw_pixel_filled_round (x, y)] draws each filled pixel for round block at 
location [(x, y)]. *)
let draw_pixel_filled_round (x, y) = 
  draw_circle (x*pixel_size + buffer + pixel_size/2) 
    (y*pixel_size + buffer + pixel_size/2) 
    (pixel_size/2 - piece_border);
  fill_circle (x*pixel_size + buffer + pixel_size/2) 
    (y*pixel_size + buffer + pixel_size/2) 
    (pixel_size/2 - piece_border)

(** [draw_pixel_round (x, y)] draws a circular pixel at the specified 
    coordinates *)
let draw_pixel_round (x, y) = 
  draw_pixel_frame_round (x, y);
  draw_pixel_filled_round (x, y)

let draw_block_round (block:block) = 
  set_color (( !color_palette) (block.color));  
  ignore(List.map (fun (x,y) -> draw_pixel_round (x, y)) (block_pixels block))

(** [draw_ghost_frame] draws the frame for a ghost piece, which is slightly 
    smaller than a regular frame to distinguish *)
let draw_ghost_frame_round (x, y) = 
  draw_circle (x*pixel_size + buffer + pixel_size/2) 
    (y*pixel_size + buffer + pixel_size/2) 
    (pixel_size/2 - piece_border) 

let draw_ghost_piece_round (ghost : block) = 
  set_color (( !color_palette) (ghost.color));
  ignore(List.map (fun (x,y) -> draw_ghost_frame_round (x, y)) 
           (block_pixels ghost))

(** [draw_pixel_frame_abs_round (x, y)] draws a circular pixel frame at the 
specified coordinates for round blocks. *)
let draw_pixel_frame_abs_round (x, y) = 
  draw_circle (x + pixel_size/2) (y + pixel_size/2) (pixel_size/2)

(** [draw_pixel_filled_abs_round (x, y)] draws a circular filled pixel at the 
specified coordinates for round blocks. *)
let draw_pixel_filled_abs_round (x, y) = 
  draw_circle (x + pixel_size/2) (y + pixel_size/2) 
    (pixel_size/2 - piece_border);
  fill_circle (x + pixel_size/2) (y + pixel_size/2) 
    (pixel_size/2 - piece_border)

(** [draw_pixel_abs_round (x, y)] draws a circular pixel at the specified 
    coordinates for round blocks. *)
let draw_pixel_abs_round (x, y) = 
  draw_pixel_frame_abs_round (x, y);
  draw_pixel_filled_abs_round (x, y)

(** [draw_block_abs_round (x, y)] draws a block at the specified 
    coordinates for round blocks. *)
let draw_block_abs_round (block:block) pixels = 
  set_color (( !color_palette) (block.color));  
  ignore(List.map (fun (x,y) -> draw_pixel_abs_round (x, y)) pixels)

(** [draw_first_round block] is the list of coordinates for the block [block] 
    which is first in queue when the current block shape is round. *)
let draw_first_round queue = 
  draw_block_abs_round (List.hd queue) (first_queue_fy (List.hd queue))

(** [draw_second_round block] is the list of coordinates for the block [block] 
    which is second in queue when the current block shape is round. *)
let draw_second_round queue = 
  draw_block_abs_round (List.nth queue 1) (snd_queue_fy (List.nth queue 1))

(** [draw_third_round block] is the list of coordinates for the block [block] 
    which is third in queue when the current block shape is round. *)
let draw_third_round queue =
  draw_block_abs_round (List.nth queue 2) (thrd_queue_fy (List.nth queue 2))

let draw_queue_round (queue : block list) =
  set_color (( !color_palette) White);
  draw_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  fill_rect (!screen_width*pixel_size+buffer*2) 
    (buffer+(!screen_height-sidebar_h)*pixel_size) 
    (sidebar_w*pixel_size) (sidebar_h*pixel_size);
  draw_first_round queue;
  draw_second_round queue;
  draw_third_round queue

let draw_held_piece_round (piece : block option) = 
  set_color (( !color_palette) White);
  draw_rect (!screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size);
  fill_rect (!screen_width*pixel_size+buffer*2) 
    ((!screen_height-sidebar_h-holdbar_h)*pixel_size) 
    (sidebar_w*pixel_size) (holdbar_h*pixel_size);
  match piece with
  | None -> ()
  | Some block -> draw_block_abs_round block (draw_held_piece_helper block)

let draw_screen_round screen = 
  set_color (( !color_palette) White);
  draw_rect buffer buffer 
    (!screen_width*pixel_size) (!screen_height*pixel_size);
  fill_rect buffer buffer 
    (!screen_width*pixel_size) (!screen_height*pixel_size);
  for x = 0 to (!screen_width - 1) do 
    for y = 0 to (!screen_height - 1) do 
      let (x', y') = block_to_screen_pos (x, y) in 
      let color = screen.(x').(y') in
      set_color (( !color_palette) (color));
      if color != White then
        draw_pixel_round (x, y) else ()
    done 
  done

(************************ ROUND BOIS END ************************)

(************************ SUMMARY FUNCS ************************)

let draw_active_screen_rec state = 
  draw_screen state.screen;
  draw_queue state.queue; 
  draw_block state.cur_block;
  draw_ghost_piece (move_tetro_down_hard state).cur_block;
  draw_held_piece state.held_block;
  draw_borders state;
  draw_score state.score;
  draw_word (((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*22), 
             square_size*3) square_size (classic Red) (str_to_list "pause");
  ()

let draw_active_screen_round state = 
  draw_screen_round state.screen;
  draw_queue_round state.queue; 
  draw_block_round state.cur_block;
  draw_ghost_piece_round (move_tetro_down_hard state).cur_block;
  draw_held_piece_round state.held_block;
  draw_borders state;
  draw_score state.score;
  (* Pause button *)
  draw_word (((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*22), 
             square_size*3) 
    square_size (classic Red) (str_to_list "pause");
  ()
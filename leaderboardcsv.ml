open Unix
open Graphics
open Gamesetup
open Drawfuncs

type score_info = string * int * string
exception FormatError

let tuple_lst = ref []

let game_over_score_info state =
  let todays_date = 
    let time_rec = localtime (time ()) in
    let day = time_rec.tm_mday in
    let month = (time_rec.tm_mon + 1) in
    let year = (time_rec.tm_year + 1900) in
    (string_of_int day)^"/"^(string_of_int month)^"/"^(string_of_int year) in
  (state.user_name, state.score, todays_date)

let write_to_csv fname =
  let wfile = open_out fname in
  let iter = (List.length !tuple_lst) - 1 in
  for i=0 to iter do
    let name, score, date = List.nth !tuple_lst i in
    output_string wfile (name^","^(string_of_int score)^","^date^"\n");
    flush wfile
  done

let read_from_csv fname =
  let rfile = open_in fname in
  let lst_to_tuple three_elem_lst =
    match three_elem_lst with
    | a::b::c::[] -> (a, (int_of_string b), c)
    | _ -> raise FormatError in
  try (
    tuple_lst := [];
    while true do
      let rline = input_line rfile in
      let stats_lst = String.split_on_char ',' rline in
      tuple_lst := !tuple_lst @ [(lst_to_tuple stats_lst)];
    done;
  ) with _ -> close_in_noerr rfile

(** [new_top_scorers] is the top 10 highest scorer's information. *)
let new_top_scorers () =
  (* sorts list of (user_name * score * date) *)
  let compare_snd (_,score1,_) (_,score2,_) =
    let compare_fst = compare score1 score2 in
    if compare_fst <> 0 then (-1*compare_fst)
    else -1 in
  let rec first_nxt nxt lst =
    match lst with
    | [] -> []
    | h::t -> if nxt = 1 then [h] else h::(first_nxt (nxt-1) t) in
  first_nxt 10 (List.sort compare_snd !tuple_lst)

let add_cur_score_info score_info =
  tuple_lst := !tuple_lst @ [score_info]

let update_top_score_info () =
  tuple_lst := new_top_scorers () 

let get_tuple_lst () =
  read_from_csv "leaderboard.csv"; !tuple_lst

let renew_leaderboard state = 
  let cur_score_info = game_over_score_info state in
  read_from_csv "leaderboard.csv";
  add_cur_score_info cur_score_info;
  update_top_score_info ();
  write_to_csv "leaderboard.csv";
  {state with refresh = false}

let draw_leaderboard state =
  let score_info_lst = get_tuple_lst in
  draw_word (square_size*4, (!screen_height*pixel_size)-(square_size*7)) 
    square_size (classic Blue) (str_to_list "leaderboard:");
  set_color black;
  for i=0 to ((List.length (score_info_lst ()))-1) do
    let name, score, date = List.nth (score_info_lst ()) i in
    moveto (buffer*5) (!screen_height*pixel_size - buffer*(12+3*i));
    draw_string name;
    moveto (buffer*30) (!screen_height*pixel_size - buffer*(12+3*i));
    draw_string (string_of_int score);
    moveto (buffer*40) (!screen_height*pixel_size - buffer*(12+3*i));
    draw_string date;
  done;

  draw_word (square_size*4, square_size*25) square_size (classic Black) 
    (str_to_list "your score:");
  draw_word (square_size*50, square_size*25) square_size (classic Blue) 
    (str_to_list (string_of_int state.score));
  draw_word (((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*19), 
             square_size*4) square_size (classic Red) (str_to_list "exit");
  state

let parse_exit_leaderboard () =
  parse_button ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*19)
    ((!screen_width+sidebar_w)*pixel_size+buffer*3-square_size*4)
    (square_size*4) (square_size*9)

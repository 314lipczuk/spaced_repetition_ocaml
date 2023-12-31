open Bogue
module W = Widget
module L = Layout

let init_database () = 
  if not @@ Sys.file_exists "test.db" then
  (
    print_endline "Database not found. Creating...";
    let db = Sqlite3.db_open "test.db" in
    let _ = Sqlite3.exec db 
      "CREATE TABLE question ( id_question INTEGER PRIMARY KEY, question string not null, answer string not null );
      CREATE TABLE answer (id_answer INTEGER PRIMARY KEY, id_fk_question int not null, scheduled_at int not null, is_correct int null, streak int null, FOREIGN KEY( id_fk_question) references question (id_question));"
    in ())
  else ()

let _ = init_database ()
let db = Sqlite3.db_open "test.db"
let create_question question answer =  
  let open Sqlite3 in
  let day = float_of_int ( 60 * 60 * 24 ) in
  let insert_q = (Printf.sprintf "insert into question (question, answer) values ('%s', '%s');" question answer)
  |> exec db in
  match insert_q with
  | Rc.OK -> (
    let _ = Printf.sprintf "insert into answer (id_fk_question, scheduled_at, streak) values (%d, %d, %d); " 
      ( Int64.to_int (last_insert_rowid db ))   (* fk to last row  *)
      ( Int.of_float @@ Unix.time () +. day )   (* scheduled time  *)
      0                                         (* streak          *)
    |> exec db
    in ())
  | _ -> print_endline (Sqlite3.Rc.to_string insert_q)

let get_next_schedule_date streak = 
  let day = float_of_int ( 60 * 60 * 24 ) in
  let now = Unix.time () in
  let multiplier = if streak < 7 then
     (2. ** Float.of_int streak)
  else
    64.
  in
  Int.of_float @@ now +. (day *. multiplier)

let save_answer ~is_good ~questiton_id ~current_streak ~id_qualifier_answer = 
  let query = String.cat
     (Printf.sprintf "update answer set is_correct = %d where id_answer = %d;" (if is_good then 1 else 0) id_qualifier_answer)
     (Printf.sprintf "insert into answer ( if_fk_question, scheduled_at, streak) values (%d, %d, %d);" questiton_id (get_next_schedule_date current_streak) (current_streak + 1)  ) 
  in
  let _ = Sqlite3.exec db query in ()

let question_gui row _ =
  (*Could also do like: let q = Some row.(0) in *)
  let _ = print_endline "in callback" in
  match row.(0), row.(1), row.(2), row.(3), row.(4) with 
  | Some question, Some answer, Some id_question, Some id_answer, Some streak -> (
    let question_label = W.text_display ~w:40 ~h:40 question in
    let input = W.text_input ~max_size:10 ~prompt:"Enter your question" () in
    let your_ans_area = W.text_display ~w:100 ~h:50 "" in
    let proper_ans_area = W.text_display ~w:100 ~h:50 "" in
    let good_button= W.button "good" ~action: (fun st -> (save_answer ~is_good:st ~questiton_id:(Stdlib.int_of_string id_question) ~current_streak:(Stdlib.int_of_string streak) ~id_qualifier_answer:(Stdlib.int_of_string id_answer ))) in
    let bad_button = W.button "bad" ~action:(fun st -> (save_answer ~is_good:st ~questiton_id:(Stdlib.int_of_string id_question) ~current_streak:(Stdlib.int_of_string streak) ~id_qualifier_answer:(Stdlib.int_of_string id_answer ))) in
    let layout = L.tower [
      L.resident ~w:400 ~h:30 question_label ;
      L.resident ~w:400 input;
      L.resident ~w:400 your_ans_area;
      L.resident ~w:400 proper_ans_area;
      L.flat_of_w [
        good_button;
        bad_button
      ]] in
    let answer_action _ dst _ = W.set_text dst answer  in
    let update_text ti l _ = 
      let text = W.get_text ti in
      W.set_text l text  in
    let c = W.connect input your_ans_area update_text Trigger.[text_input;key_down ] in
    let c2 = W.connect good_button proper_ans_area answer_action Trigger.[key_down] in
    let c3 = W.connect bad_button proper_ans_area answer_action Trigger.[key_down] in
    let board = Bogue.make [c;c2;c3] [layout] in
    Bogue.run board )
  | _ -> ()

let get_todays_questions () = 
  let query = Printf.sprintf "select q.question, q.answer, q.id_question, a.id_answer, a.streak from question as q inner join answer as a on q.id_question = a.id_fk_question where is_correct IS NULL and a.scheduled_at > '%d';" @@ Int.of_float @@ Unix.time () in
  Sqlite3.exec ~cb:question_gui db query

let _browse_questions () = 
  let input = W.text_input ~max_size:200 ~prompt:"Enter your question" () in
  let label = W.text_display ~w:40 ~h:40 "How to add questions?" in
  let layout = L.tower [
    L.resident ~w:400 input;
    L.resident ~w:400 ~h:200 label
  ] in
  let action ti l _ = 
    let text = W.get_text ti in
    W.set_text l ("Hello " ^ text ^ "!") in
  let c = W.connect input label action Trigger.[text_input;key_down] in
  let board = Bogue.make [c] [layout] in
  Bogue.run board

let _main_gui () = 
  let input = W.text_input ~max_size:200 ~prompt:"Enter your question" () in
  let label = W.text_display ~w:40 ~h:40 "How to add questions?" in
  let layout = L.tower [
    L.resident ~w:400 input;
    L.resident ~w:400 ~h:200 label
    ] in
  let action ti l _ = 
    let text = W.get_text ti in
    W.set_text l ("Hello " ^ text ^ "!") in
  let c = W.connect input label action Trigger.[text_input;key_down ] in
  let board = Bogue.make [c] [layout] in
  Bogue.run board

type _question_record = {
  question:string;
  answer:string;
  }

let add_question_gui () = 
  let input = W.text_input ~max_size:1000 ~prompt:"Enter your question" () in
  let formatted_result = W.text_display ~w:40 ~h:40 "" in
  let input_answer = W.text_input ~max_size:1000 ~prompt:"Enter your answer" () in
  let formatted_result_answer = W.text_display ~w:40 ~h:40 "" in
  let layout = L.tower [
    L.resident ~w:400 ~h:50 input ;
    L.resident ~w:400 ~h:100 formatted_result ;
    L.resident ~w:400 ~h:50 input_answer ;
    L.resident ~w:400 ~h:100 formatted_result_answer ;
    ] in
  let update_in source destination event =
    let open Tsdl.Sdl in
    let _x = match Trigger.event_kind event with
    | `Key_down -> (
      match Event.get event Event.keyboard_keycode with
      | c when c = K.return -> create_question (W.get_text formatted_result ) (W.get_text formatted_result_answer ); ()
      | _ -> ())
    | _ -> (); in
    let text = W.get_text source in
    W.set_text destination text
    in
  let question_connection = W.connect input formatted_result update_in Trigger.[text_input;key_down] in
  let answer_connection = W.connect input_answer formatted_result_answer update_in Trigger.[text_input;key_down] in
  let board = Bogue.make [question_connection; answer_connection] [layout] in
  Bogue.run board

let () = 
  let _ = get_todays_questions () in
  add_question_gui () ;
  Bogue.quit ()

(*
let () = 
  let _ = question_gui (Array.of_list [(Some "asdf"); (Some "fff")] ) () in
  Bogue.quit ()
*)

(* 
 Aight, what are the components?   
  sqlite module:
    - check if its time for answer/s
    - answer & check
    - store question & answer
    - compute dates based on answer

  gui module:
    main screen:
      show answerables, button "add question", browse? / cal
    add_question: 
      text inputs for question & answer -> store
    answer:
      show question, give text input, button for submit;
      then compare the two answers side-by-side
*)

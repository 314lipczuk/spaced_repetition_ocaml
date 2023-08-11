open Bogue

let _db () = Sqlite3.db_open "test.db"

module W = Widget
module L = Layout

let _gui () = 
  let input = W.text_input ~max_size:200 ~prompt:"Enter your question" () in
  let label = W.text_display ~w:40 ~h:40 "How to add questions?" in
  let layout = L.tower [
    L.resident ~w:400 input;
    L.resident ~w:400 ~h:200 label ] in
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

let _add_question_gui () = 
  let input = W.text_input ~max_size:100 ~prompt:"Enter your question" () in
  let formatted_result = W.text_display ~w:40 ~h:40 "" in
  let input_answer = W.text_input ~max_size:100 ~prompt:"Enter your answer" () in
  let formatted_result_answer = W.text_display ~w:40 ~h:40 "" in
  let layout = L.tower [
    L.resident ~w:400 ~h:50 input ;
    L.resident ~w:400 ~h:200 formatted_result ;
    L.resident ~w:400 ~h:50 input_answer ;
    L.resident ~w:400 ~h:200 formatted_result_answer ;
    ] in
  let update_in source destination event =
    let open Tsdl.Sdl in
    match Trigger.event_kind event with
    | `Key_down -> (
      match Event.get event Event.keyboard_keycode with
      | c when c = K.return -> print_endline "Return";()
      | _ -> print_endline "Other key")
    | `Text_input -> print_endline "Text input"
    | _ -> () ;
    let text = W.get_text source in
    W.set_text destination ("Q: " ^ text )
    in
  let question_connection = W.connect input formatted_result update_in Trigger.[text_input;key_down] in
  let answer_connection = W.connect input_answer formatted_result_answer update_in Trigger.[text_input;key_down] in
  let board = Bogue.make [question_connection; answer_connection] [layout] in
  Bogue.run board

let () = _gui();
  Bogue.quit ()

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



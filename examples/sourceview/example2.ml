(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    There is no specific licensing policy, but you may freely           *)
(*    take inspiration from the code, and copy parts of it in your        *)
(*    application.                                                        *)
(*                                                                        *)
(**************************************************************************)
(* Run with ../../src/lablgtk2 -localdir example2.ml  *)

(* Alternatively, run with
  opam install ocamlfind lablgtk conf-gtksourceview
  ocamlfind ocamlopt -unsafe-string -package lablgtk2.sourceview2 -linkpkg -o example2 example2.ml
  ./example2

  TODO: replace String with Bytes.
*)

open Printf

let print_infos = false
let print_spaces = false

let lang_mime_type = "text/x-ocaml"
let lang_name = "ocaml"
let use_mime_type = true
let font_name = "Monospace 10"

let print_lang lang = prerr_endline (sprintf "language: %s" lang#name)

let print_lang_ids language_manager =
  let i = ref 0 in
  prerr_endline "language_ids:";
  List.iter
    (fun id -> incr i;
      match language_manager#language id with
	Some lang ->
          let name = lang#name in
          let section = lang#section in
          prerr_endline
   	    (sprintf "%d: %s %s (%s)" !i id name section)
        | None -> ())
    language_manager#language_ids

let print_style_schemes mgr =
  let i = ref 0 in
  prerr_endline "style schemes:";
  List.iter (fun id ->
      incr i;
      match mgr#style_scheme id with
          Some scm ->
            prerr_endline
	      (sprintf "%d: %s %s" !i id scm#description)
        | None -> ())
    mgr#style_scheme_ids

let locale = GtkMain.Main.init ()

let win = GWindow.window ~title:"LablGtkSourceView test" ()
let scrolled_win = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing:win#add ()

let source_view =
  GSourceView2.source_view
    ~auto_indent:true
    ~insert_spaces_instead_of_tabs:true ~tab_width:2
    ~show_line_numbers:true
    ~right_margin_position:80 ~show_right_margin:true
    (* ~smart_home_end:true *)
    ~packing:scrolled_win#add ~height:500 ~width:650
    ()

let language_manager = GSourceView2.source_language_manager ~default:true

let lang =
  if use_mime_type then
    (match language_manager#guess_language ~content_type:lang_mime_type () with
        Some x -> x
      | None -> failwith (sprintf "no language for %s" lang_mime_type))
  else
    (match language_manager#language lang_name with
        Some x -> x
      | None -> failwith (sprintf "can't load %s" lang_name))


let () =
  if print_infos then begin
    print_lang_ids language_manager;
    print_lang lang;
    let style_scheme_manager =
      GSourceView2.source_style_scheme_manager ~default:true in
    print_style_schemes style_scheme_manager
  end

exception Cancel

(** Text description of the unicode bindings:

    one item per line, each item consists of:
    - a leading backslahs
    - a ascii word next to it
    - a unicode word (or possibly a full sentence in-between doube-quotes,
     the sentence may include spaces and \n tokens),
    - optinally, an integer indicating the "priority" (lower is higher priority),
      technically the length of the prefix that suffices to obtain this word.
      Ex. if "\lambda" has priority 3, then "\lam" always decodes as "\lambda".

      \pi π
      \lambda λ 3
      \lambdas λs 4
      \lake Ο 2
      \lemma "Lemma foo : x. Proof. Qed." 1

    - In case of equality between two candidates (same ascii word, or same
      priorities for two words with similar prefix), the first binding is considered.
*)

let unicode_bindings = [
  ("\\pi", "π", None);
  ("\\lambdas", "λs", Some 4);
  ("\\lambda", "λ", Some 3);
  ("\\lake", "0", Some 2);
  ("\\lemma", "Lemma foo : x. Proof. Qed", Some 1);
  ]

(** Auxiliary function to test whether [s] is a prefix of [str] *)

let string_is_prefix s str =
   let n = String.length s in
   let m = String.length str in
   if m < n
      then false
      else (s = String.sub str 0 n)

(* unicode_lookup : string -> string option *)

let unicode_lookup prefix =
  let max_priority = 100000000 in
  let cur_word = ref None in
  let cur_prio = ref (max_priority+1) in
  let test_binding (key, word, prio_opt) =
    let prio =
      match prio_opt with
      | None -> max_priority
      | Some p -> p
      in
    if string_is_prefix prefix key && prio < !cur_prio then begin
      cur_word := Some word;
      cur_prio := prio;
    end in
  List.iter test_binding unicode_bindings;
  !cur_word

let () =
  let text =
    let ic = open_in "example2.ml" in
    let size = in_channel_length ic in
    let buf = Bytes.create size in
    really_input ic buf 0 size;
    close_in ic;
    buf
  in
  win#set_allow_shrink true;
  source_view#misc#modify_font_by_name font_name;

  let buffer = source_view#source_buffer in
  buffer#set_highlight_matching_brackets true;
  buffer#set_language (Some lang);
  buffer#set_highlight_syntax true;

  source_view#set_smart_home_end `AFTER;
  if source_view#smart_home_end <> `AFTER then failwith "regret";

  if print_spaces then begin
    source_view#set_draw_spaces [`SPACE; `NEWLINE];
    List.iter
      (function
        | `SPACE -> print_string " space"
        | `TAB -> print_string " tab"
        | `NEWLINE -> print_string " newline"
        | `NBSP -> print_string " nbsp"
        | `LEADING -> print_string "leading"
        | `TEXT -> print_string "text"
        | `TRAILING -> print_string "trailing")
      source_view#draw_spaces;
  end;

  print_newline ();

  ignore (buffer#connect#changed (fun _ ->
    prerr_endline "changed";
    try

      let m_insert = buffer#get_mark `INSERT in
      let i_insert = buffer#get_iter_at_mark (`MARK m_insert) in
      (* alternative:
      let i_insert = buffer#get_iter_at_mark `INSERT in
      let m_insert = buffer#create_mark ~left_gravity:false i_insert in*)

      let i_line_start = i_insert#backward_line in

      (*debug:
      let s = i_line_start#get_slice i_insert in
      Printf.eprintf "Line=%s" s;
      prerr_endline "";*)

      let r_backslash = i_insert#backward_search ~limit:i_line_start "\\" in
      let i_backslash_start =
        match r_backslash  with
        | None -> raise Cancel
        | Some (i_backslash_start,i_backslash_stop) -> i_backslash_start
        in

      let code = i_backslash_start#get_text i_insert in
      (*debug:
      Printf.eprintf "Code=%s" code;
      prerr_endline "";*)

      let n = String.length code in
      if n = 0 then raise Cancel;
      if code.[n-1] <> '!' then raise Cancel;
      let prefix = String.sub code 0 (n-1) in
      let word =
        match unicode_lookup prefix with
        | None -> prerr_endline "Warning: no binding found."; raise Cancel
        | Some w -> w
        in

      ignore (buffer#delete ~start:i_backslash_start ~stop:i_insert);
      let i_insert_new = buffer#get_iter_at_mark (`MARK m_insert) in
      ignore (buffer#insert_interactive ~iter:i_insert_new word);

      (* http://lablgtk.forge.ocamlcore.org/refdoc/GText.buffer_skel-c.html *)

    with Cancel -> ()
   ));

  ignore (win#connect#destroy (fun _ -> GMain.quit ()));
  ignore (source_view#connect#undo (fun _ -> prerr_endline "undo"));

  buffer#begin_not_undoable_action ();
  buffer#set_text text;
  buffer#end_not_undoable_action ();

  win#show ();
  GMain.Main.main ()


(*
let template_item (text, offset, len, key) =
  let modifier = modifier_for_templates#get in
  let idx = String.index text ' ' in
  let name = String.sub text 0 idx in
  let label = "_"^name^" __" in
  let negoffset = String.length text - offset - len in
  let callback sn =
    let b = sn.buffer in
    if b#insert_interactive text then begin
      let iter = b#get_iter_at_mark `INSERT in
      ignore (iter#nocopy#backward_chars negoffset);
      b#move_mark `INSERT ~where:iter;
      ignore (iter#nocopy#backward_chars len);
      b#move_mark `SEL_BOUND ~where:iter;
    end

*)
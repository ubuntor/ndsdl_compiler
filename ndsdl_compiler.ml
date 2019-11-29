open Core

let main ~input_file ~output_file =
  let _output_file = Option.value ~default:(input_file ^ ".out") output_file in
  match Parse.lex_and_parse input_file with
  | Some parsed ->
     Printf.printf !"Parsed successfully: %{sexp:Ndsdl.Formula.t}\n" parsed;
     Check.check_formula parsed;
     Printf.printf !"Checked successfully\n"
  | None -> Printf.printf !"Failed to parse\n"

let command =
  Command.basic ~summary:"NDSdL-to-dL compiler"
    ~readme:(fun () -> "Compiles NDSdL down to dL parsable by KeYmaera X")
    Command.Let_syntax.(
      let%map_open input_file = anon ("filename" %: Filename.arg_type)
      and output_file =
        flag "-o"
          (optional Filename.arg_type)
          ~doc:"filename Output filename. Defaults to \"input_filename.out\"."
      in
      fun () -> main ~input_file ~output_file)

let () = Command.run command

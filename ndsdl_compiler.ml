open Core

let main ~input_file ~output_file =
  let output_file = Option.value ~default:(input_file ^ ".kyx") output_file in
  match Parse.lex_and_parse input_file with
  | Some parsed ->
      Printf.printf
        !"Parsed successfully: %{sexp:Ndsdl_extra.Formula.t}\n"
        parsed;
      let desugared = Ndsdl_extra_to_ndsdl.translate_formula parsed in
      Printf.printf !"Desugared: %{sexp:Ndsdl.Formula.t}\n" desugared;
      let translated = Ndsdl_to_dl.translate_formula desugared in
      Printf.printf !"Translated: %{sexp:Dl.Formula.t}\n" translated;
      Output.output_to_file translated ~output_file;
      Printf.printf !"Wrote to %s\n" output_file
  | None -> Printf.printf !"Failed to parse\n"

let command =
  Command.basic ~summary:"NDSdL-to-dL compiler"
    ~readme:(fun () -> "Compiles NDSdL down to dL parsable by KeYmaera X")
    Command.Let_syntax.(
      let%map_open input_file = anon ("filename" %: Filename.arg_type)
      and output_file =
        flag "-o"
          (optional Filename.arg_type)
          ~doc:"filename Output filename. Defaults to \"input_filename.kyx\"."
      in
      fun () -> main ~input_file ~output_file)

let () = Command.run command

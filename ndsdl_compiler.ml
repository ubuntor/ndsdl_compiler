open Core

let main ~input_file ~output_file ~prob_var =
  let output_file = Option.value ~default:(input_file ^ ".kyx") output_file in
  let prob_var = Option.value ~default:"p" prob_var in
  match Parse.lex_and_parse input_file with
  | Some parsed ->
      Printf.printf !"Parsed successfully: %{sexp:Ndsdl.Formula.t}\n" parsed;
      let translated = Trans.translate_formula parsed ~prob_var in
      Printf.printf !"Translated: %{Dl.Formula}\n" translated;
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
      and prob_var =
        flag "-p" (optional string)
          ~doc:
            "prob_var Probability variable. Must not be assigned to. Defaults \
             to \"p\"."
      in
      fun () -> main ~input_file ~output_file ~prob_var)

let () = Command.run command

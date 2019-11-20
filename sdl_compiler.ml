open Core

let main ~input_file ~output_file =
  let output_file = Option.value ~default:(input_file ^ ".out") output_file in
  Printf.printf !"%s %s" input_file output_file

let command =
  Command.basic ~summary:"SdL-to-dL compiler"
    ~readme:(fun () -> "Compiles SdL down to dL parsable by KeYmaera X")
    Command.Let_syntax.(
      let%map_open input_file = anon ("filename" %: Filename.arg_type)
      and output_file =
        flag "-o"
          (optional Filename.arg_type)
          ~doc:"filename Output filename. Defaults to \"input_filename.out\"."
      in
      fun () -> main ~input_file ~output_file)

let () = Command.run command

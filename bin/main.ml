module Cli = Lsp.Cli

let () =
  Printexc.record_backtrace true;
  let arg = Lsp.Cli.Arg.create () in
  let spec = Cli.Arg.spec arg in
  let usage =
    "atdlsp [ --stdio | --socket PORT | --port PORT | --pipe PIPE ] [ \
     --clientProcessId pid ]"
  in
  Arg.parse spec
    (fun _ -> raise @@ Arg.Bad "anonymous arguments aren't allowed")
    usage;
  let channel =
    match Cli.Arg.channel arg with
    | Ok c -> c
    | Error s ->
        Format.eprintf "%s@.%!" s;
        Arg.usage spec usage;
        exit 1
  in
  try Atd_lsp_lib.run channel
  with exn ->
    prerr_endline @@ Printexc.to_string exn;
    Printexc.print_backtrace stderr;
    exit 1

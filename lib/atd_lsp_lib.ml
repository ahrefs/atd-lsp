module Io_lwt =
  Lsp.Io.Make
    (struct
      type 'a t = 'a Lwt.t

      let return = Lwt.return
      let raise = Lwt.fail

      module O = struct
        let ( let+ ) p f = Lwt.map f p
        let ( let* ) p f = Lwt.bind p f
      end
    end)
    (struct
      type input = Lwt_io.input_channel
      type output = Lwt_io.output_channel

      let read_line = Lwt_io.read_line_opt

      let read_exactly ic count =
        let buf = Bytes.create count in
        try%lwt
          let%lwt () = Lwt_io.read_into_exactly ic buf 0 count in
          Lwt.return_some (Bytes.to_string buf)
        with End_of_file -> Lwt.return_none

      let write oc lines =
        Lwt_list.iter_s (fun line -> Lwt_io.write oc line) lines
    end)

module Position_helper = struct
  (* from ocaml-lsp-server/src/position.ml *)

  include Lsp.Types.Position

  let is_dummy (lp : Lexing.position) =
    lp.pos_lnum = Lexing.dummy_pos.pos_lnum
    && lp.pos_cnum = Lexing.dummy_pos.pos_cnum

  let of_lexical_position (lex_position : Lexing.position) : t =
    let line = lex_position.pos_lnum - 1 in
    let character = lex_position.pos_cnum - lex_position.pos_bol in
    let line = max line 0 in
    let character = max character 0 in
    { line; character }

  let to_lsp_range start end_ =
    let start = of_lexical_position start in
    let end_ = of_lexical_position end_ in
    { Lsp.Types.Range.start; end_ }
end

module Symbols = struct
  type t = {
    symbols : (string * Atd.Loc.t) list;
    declarations : (string * Atd.Loc.t) list;
  }

  let of_atd_string s =
    let m, _ = Atd.Util.load_string s in

    (* Collect all named entities and their locations *)
    let symbols = ref [] in
    let declarations = ref [] in

    (* Process type definitions *)
    let process_type_def k td =
      let loc, (name, _, _), _ = td in
      declarations := (name, loc) :: !declarations;
      k td
    in

    (* Process type expressions *)
    let process_type_expr k te =
      match te with
      | Atd.Ast.Name (loc, (_, name, _), _) ->
          symbols := (name, loc) :: !symbols;
          k te
      | _ -> k te
    in

    (* Process fields *)
    let process_field k field =
      match field with
      | `Inherit _ -> ()
      | `Field (loc, (name, _, _), _) ->
          symbols := (name, loc) :: !symbols;
          k field
    in

    (* Process variants *)
    let process_variant k variant =
      match variant with
      | Atd.Ast.Inherit _ -> ()
      | Atd.Ast.Variant (loc, (name, _), _) ->
          symbols := (name, loc) :: !symbols;
          k variant
    in

    (* Collect all tokens *)
    let () =
      Atd.Ast.visit ~type_def:process_type_def ~type_expr:process_type_expr
        ~field:process_field ~variant:process_variant () (Full_module m)
    in
    { symbols = !symbols; declarations = !declarations }

  let at_position (s : t) position =
    try
      (* Convert the LSP position to a lexical position for comparison *)
      let target_line = position.Lsp.Types.Position.line + 1 in
      let target_char = position.Lsp.Types.Position.character in

      (* Find a token that contains the position *)
      let matching_token =
        List.find_opt
          (fun (_name, (start_pos, end_pos)) ->
            let start_line = start_pos.Lexing.pos_lnum in
            let start_char =
              start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol
            in
            let end_line = end_pos.Lexing.pos_lnum in
            let end_char = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in

            (* Check if position is within the token's range *)
            (target_line > start_line
            || (target_line = start_line && target_char >= start_char))
            && (target_line < end_line
               || (target_line = end_line && target_char <= end_char)))
          s.symbols
      in

      match matching_token with Some (name, _) -> Some name | None -> None
    with Atd.Ast.Atd_error _ | Parsing.Parse_error -> None

  let declaration_position (s : t) name =
    List.find_opt (fun (n, _) -> n = name) s.declarations
end

module Requests = struct
  let document_symbol (req : Lsp.Types.DocumentSymbolParams.t) =
    let doc = req.textDocument in
    let uri = doc.uri in
    let path = Lsp.Uri.to_path uri in
    let%lwt atd_string =
      Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic -> Lwt_io.read ic)
    in
    let { Symbols.declarations; _ } = Symbols.of_atd_string atd_string in
    let symbols =
      declarations
      |> List.map (fun (name, (start, end_)) ->
             let range = Position_helper.to_lsp_range start end_ in
             let kind = Lsp.Types.SymbolKind.TypeParameter in
             Lsp.Types.DocumentSymbol.create ~name ~kind ~range
               ~selectionRange:range ())
    in
    Lwt.return_some (`DocumentSymbol symbols)

  let definition (req : Lsp.Types.DefinitionParams.t) =
    let doc = req.textDocument in
    let uri = doc.uri in
    let path = Lsp.Uri.to_path uri in

    let position = req.position in

    let%lwt atd_string =
      Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic -> Lwt_io.read ic)
    in
    let symbols = Symbols.of_atd_string atd_string in
    match Symbols.at_position symbols position with
    | Some word -> (
        match Symbols.declaration_position symbols word with
        | Some (_, (start, end_)) ->
            let range = Position_helper.to_lsp_range start end_ in
            let location = Lsp.Types.Location.create ~uri ~range in
            Lwt.return_some (`Location [ location ])
        | None -> Lwt.return_none)
    | None -> Lwt.return_none

  let initialize () =
    let capabilities =
      Lsp.Types.ServerCapabilities.create ~documentSymbolProvider:(`Bool true)
        ~definitionProvider:(`Bool true) ()
    in
    Lwt.return @@ Lsp.Types.InitializeResult.create ~capabilities ()
end

let on_request (req : Jsonrpc.Request.t) : Jsonrpc.Response.t Lwt.t =
  match Lsp.Client_request.of_jsonrpc req with
  | Error message ->
      let code = Jsonrpc.Response.Error.Code.InvalidParams in
      let error = Jsonrpc.Response.Error.make ~code ~message () in
      Lwt.return (Jsonrpc.Response.error req.id error)
  | Ok (Lsp.Client_request.E r) -> (
      match r with
      | DocumentSymbol s ->
          let%lwt res = Requests.document_symbol s in
          let response = Lsp.Client_request.yojson_of_result r res in
          Lwt.return (Jsonrpc.Response.ok req.id response)
      | TextDocumentDefinition s ->
          let%lwt res = Requests.definition s in
          let response = Lsp.Client_request.yojson_of_result r res in
          Lwt.return (Jsonrpc.Response.ok req.id response)
      | Initialize _ ->
          let%lwt res = Requests.initialize () in
          let response = Lsp.Client_request.yojson_of_result r res in
          Lwt.return (Jsonrpc.Response.ok req.id response)
      | Shutdown ->
          let response = Lsp.Client_request.yojson_of_result r () in
          Lwt.return (Jsonrpc.Response.ok req.id response)
      | _ -> Lwt.fail_with "not implemented")

let handler (ic, oc) =
  match%lwt Io_lwt.read ic with
  | None -> Lwt.return_unit
  | Some packet -> (
      match packet with
      | Request r ->
          let%lwt resp = on_request r in
          Io_lwt.write oc (Jsonrpc.Packet.Response resp)
      | Notification _n ->
          prerr_endline "received notification";
          Lwt.return_unit
      | Response _r ->
          prerr_endline "received response";
          Lwt.return_unit
      | Batch_response _ ->
          prerr_endline "received batch response";
          Lwt.return_unit
      | Batch_call _ ->
          prerr_endline "received batch call";
          Lwt.return_unit)

let stream_of_channel = function
  | Lsp.Cli.Channel.Stdio ->
      let stdin = Lwt_io.stdin in
      let stdout = Lwt_io.stdout in
      (stdin, stdout)
  | _ -> failwith "not implemented"

let rec start (ic, oc) =
  let%lwt () = handler (ic, oc) in
  start (ic, oc)

let run (channel : Lsp.Cli.Channel.t) =
  prerr_endline "LSP server started";
  let ic, oc = stream_of_channel channel in
  Lwt_main.run @@ start (ic, oc)

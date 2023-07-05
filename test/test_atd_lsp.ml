open OUnit2
open Lsp.Types
open Atd_lsp_lib

let cwd = Sys.getcwd ()

(* Test document symbols functionality *)
let test_document_symbols _ctx =
  (* Create a document symbol request *)
  let uri = Lsp.Uri.of_path (cwd ^ "/artifacts/sample.atd") in
  let text_document = TextDocumentIdentifier.create ~uri in
  let params = DocumentSymbolParams.create ~textDocument:text_document () in

  (* Call the document symbol function and run it with Lwt *)
  let result = Lwt_main.run (Requests.document_symbol params) in

  (* Verify the result *)
  match result with
  | Some (`DocumentSymbol symbols) ->
      (* Check that we have symbols *)
      assert_bool "Should have symbols" (List.length symbols > 0);

      (* Check for specific symbols we know should be in the file *)
      let symbol_names = List.map (fun s -> s.DocumentSymbol.name) symbols in
      let expected_symbols =
        [
          "user_id";
          "username";
          "email";
          "timestamp";
          "user";
          "user_identifier";
          "address";
          "user_profile";
          "result";
          "user_result";
          "profile_result";
          "comment";
          "base_entity";
          "derived_entity";
          "notification";
          "activity";
        ]
      in

      List.iter
        (fun expected ->
          assert_bool
            ("Should contain symbol " ^ expected)
            (List.exists (fun name -> name = expected) symbol_names))
        expected_symbols
  | _ -> assert_failure "Expected DocumentSymbol result"

(* Define the test suite *)
let suite =
  "ATD LSP Tests" >::: [ "test_document_symbols" >:: test_document_symbols ]

(* Run the tests *)
let () = run_test_tt_main suite

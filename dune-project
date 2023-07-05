(lang dune 3.8)

(name atd_lsp)

(generate_opam_files true)

(source
 (github ahrefs/atd-lsp))

(authors "Tianyi Song <tianyi.song@ahrefs.com>")

(maintainers "Tianyi Song <tianyi.song@ahrefs.com>")

; (license LICENSE)

(documentation "https://github.com/ahrefs/atd-lsp/blob/main/README.md")

(package
 (name atd_lsp)
 (synopsis "Language Server for ATD")
 (depends
  ocaml
  dune
  atd
  lsp
  js_of_ocaml
  js_of_ocaml-ppx_deriving_json
  extlib
  lwt
  lwt_ppx
  (ocaml-format
   (and
    :dev
    (= 0.24.1)))
  (ocaml-lsp-server
   (and
    :dev
    (= 1.6.0))))
 (tags
  (topics "language server" "atd")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

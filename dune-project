(lang dune 3.12)
(using directory-targets 0.1)

(name ortografe)
(generate_opam_files true)

(package
 (name ortografe)
 (allow_empty)
 (depends ocaml dune
          bheap cmdliner core core_unix csv dream eio eio_main hyper lwt_ppx markup patience_diff ppx_jane ppxlib re uri uucp uunf uuseg uutf zipc ;; MARKER

 ))

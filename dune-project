(lang dune 3.0)
(name calculon)
(generate_opam_files true)

(version 1.0)

(source
 (github c-cube/calculon))

(license MIT)

(authors c-cube Enjolras Armael)

(package
 (name calculon)
 (depends
  ("dune"
   (>= "3.0"))
  "base-unix"
  ("ocaml"
   (>= "5.0.0"))
  ("eio" (>= "1.0"))
  ("eio_main" (>= "1.0"))
  ("irky" (>= "0.1"))
  ("irky-eio" (>= "0.1"))
  ("tls-eio" (>= "1.0"))
  ("ca-certs" (>= "0.1"))
  ("sqlite3"
   (>= "5.0.0"))
  ("logs"
   (>= "0.5.0"))
  ("yojson"
   (>= "1.7"))
  ("containers"
   (and
    (>= "3.6")
    (< "4.0")))
  "ptime"
  "stringext"
  ("re"
   (and
    (>= "1.7.2")
    (< "2.0")))
  ("odoc" :with-doc))
 (depopts iter)
 (tags
  ("irc" "bot" "factoids")))

(package
 (name calculon-web)
 (synopsis "A collection of web plugins for Calculon")
 (depends
  "dune"
  ("calculon"
   (= :version))
  ("re"
   (>= "1.7.2"))
  "uri"
  "curly"
  ("atdgen" (and (>= 2.16) (< 3.0)))
  "lambdasoup"
  ("odoc" :with-doc)))

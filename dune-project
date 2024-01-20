(lang dune 2.0)

(generate_opam_files true)

(version 0.8)

(source
 (github c-cube/calculon))

(license MIT)

(authors c-cube Enjolras Armael)

(package
 (name calculon)
 (depends
  ("dune"
   (>= "1.1"))
  "base-unix"
  "lwt"
  ("irc-client"
   (>= "0.7.0"))
  "irc-client-lwt"
  "irc-client-lwt-ssl"
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
  ("odoc" :with-doc)
  ("ocaml"
   (>= "4.08.0")))
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
  "atdgen"
  "lambdasoup"
  ("odoc" :with-doc)))

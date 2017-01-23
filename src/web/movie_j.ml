(* Auto-generated from "movie.atd" *)


type year = Movie_t.year

type maybe = Movie_t.maybe

type search_entry = Movie_t.search_entry = { s_title: maybe; s_id: string }

type search_result = Movie_t.search_result = {
  count: int;
  results: search_entry list
}

type query_entry = Movie_t.query_entry = {
  title: maybe;
  id: string;
  year: year;
  rating: float;
  plot: maybe
}

let write__2 = (
  fun ob x -> (
    let x = ( Movie_schema.Year.unwrap ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  fun p lb ->
    let x = (
      Ag_oj_run.read_string
    ) p lb in
    ( Movie_schema.Year.wrap ) x
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_year = (
  write__2
)
let string_of_year ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_year ob x;
  Bi_outbuf.contents ob
let read_year = (
  read__2
)
let year_of_string s =
  read_year (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  fun ob x -> (
    let x = ( Movie_schema.Maybe.unwrap ) x in (
      Yojson.Safe.write_string
    ) ob x)
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  fun p lb ->
    let x = (
      Ag_oj_run.read_string
    ) p lb in
    ( Movie_schema.Maybe.wrap ) x
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_maybe = (
  write__1
)
let string_of_maybe ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_maybe ob x;
  Bi_outbuf.contents ob
let read_maybe = (
  read__1
)
let maybe_of_string s =
  read_maybe (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_search_entry : _ -> search_entry -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if x.s_title <> None then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"Title\":";
      (
        write_maybe
      )
        ob x.s_title;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"imdbID\":";
    (
      Yojson.Safe.write_string
    )
      ob x.s_id;
    Bi_outbuf.add_char ob '}';
)
let string_of_search_entry ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_search_entry ob x;
  Bi_outbuf.contents ob
let read_search_entry = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_s_title = ref (None) in
    let field_s_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'T' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'I' && String.unsafe_get s (pos+5) = 'D' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_s_title := (
                (
                  read_maybe
                ) p lb
              );
            )
          | 1 ->
            field_s_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'T' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'I' && String.unsafe_get s (pos+5) = 'D' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_s_title := (
                  (
                    read_maybe
                  ) p lb
                );
              )
            | 1 ->
              field_s_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields p [| !bits0 |] [| "s_id" |];
        (
          {
            s_title = !field_s_title;
            s_id = !field_s_id;
          }
         : search_entry)
      )
)
let search_entry_of_string s =
  read_search_entry (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_list (
    write_search_entry
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Ag_oj_run.read_list (
    read_search_entry
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_search_result : _ -> search_result -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if x.count <> 0 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"totalResults\":";
      (
        Yojson.Safe.write_int
      )
        ob x.count;
    );
    if x.results <> [] then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"Search\":";
      (
        write__3
      )
        ob x.results;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_search_result ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_search_result ob x;
  Bi_outbuf.contents ob
let read_search_result = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_count = ref (0) in
    let field_results = ref ([]) in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' then (
                  1
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'R' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_count := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_results := (
                (
                  read__3
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'h' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = 'R' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 'u' && String.unsafe_get s (pos+9) = 'l' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_count := (
                  (
                    Ag_oj_run.read_int
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_results := (
                  (
                    read__3
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            count = !field_count;
            results = !field_results;
          }
         : search_result)
      )
)
let search_result_of_string s =
  read_search_result (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_query_entry : _ -> query_entry -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if x.title <> None then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"Title\":";
      (
        write_maybe
      )
        ob x.title;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"imdbID\":";
    (
      Yojson.Safe.write_string
    )
      ob x.id;
    if x.year <> None then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"Released\":";
      (
        write_year
      )
        ob x.year;
    );
    if x.rating <> 0.0 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"imdbRating\":";
      (
        Yojson.Safe.write_std_float
      )
        ob x.rating;
    );
    if x.plot <> None then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"Plot\":";
      (
        write_maybe
      )
        ob x.plot;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_query_entry ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_query_entry ob x;
  Bi_outbuf.contents ob
let read_query_entry = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_title = ref (None) in
    let field_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_year = ref (None) in
    let field_rating = ref (0.0) in
    let field_plot = ref (None) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'P' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 't' then (
                  4
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'T' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'I' && String.unsafe_get s (pos+5) = 'D' then (
                  1
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'R' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' then (
                  2
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'R' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_title := (
                (
                  read_maybe
                ) p lb
              );
            )
          | 1 ->
            field_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_year := (
                (
                  read_year
                ) p lb
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_rating := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_plot := (
                (
                  read_maybe
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'P' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 't' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'T' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'I' && String.unsafe_get s (pos+5) = 'D' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'R' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'd' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'm' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'b' && String.unsafe_get s (pos+4) = 'R' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'g' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_title := (
                  (
                    read_maybe
                  ) p lb
                );
              )
            | 1 ->
              field_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_year := (
                  (
                    read_year
                  ) p lb
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_rating := (
                  (
                    Ag_oj_run.read_number
                  ) p lb
                );
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_plot := (
                  (
                    read_maybe
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields p [| !bits0 |] [| "id" |];
        (
          {
            title = !field_title;
            id = !field_id;
            year = !field_year;
            rating = !field_rating;
            plot = !field_plot;
          }
         : query_entry)
      )
)
let query_entry_of_string s =
  read_query_entry (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

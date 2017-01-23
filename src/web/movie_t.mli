(* Auto-generated from "movie.atd" *)


type year = Movie_schema.Year.t

type maybe = Movie_schema.Maybe.t

type search_entry = { s_title: maybe; s_id: string }

type search_result = { count: int; results: search_entry list }

type query_entry = {
  title: maybe;
  id: string;
  year: year;
  rating: float;
  plot: maybe
}

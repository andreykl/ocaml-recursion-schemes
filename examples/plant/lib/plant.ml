module F = struct
  type 'a t = Root of 'a | Stalk of 'a | Fork of 'a * 'a * 'a | Bloom

  let map f = function
    | Root a -> Root (f a)
    | Stalk a -> Stalk (f a)
    | Fork (l, c, r) -> Fork (f l, f c, f r)
    | Bloom -> Bloom
end

module R = Recursion.Schemes.Make (F)

let grow seed : Action.t * Seed.t =
  let act = Action.of_int_in_range (Seed.int_in_range seed) in
  (act, Seed.inc seed)

let sow : Seed.t R.cvcoalgebra =
 fun seed ->
  let act, next = grow seed in
  if Seed.is_min seed then Root (R.Automatic next)
  else if Seed.is_max seed then Bloom
  else
    match act with
    | Action.Flower -> Bloom
    | Action.Upwards -> Stalk (R.Automatic next)
    | Action.Branch ->
        Fork
          ( R.Manual (Stalk (R.Automatic next)),
            R.Manual Bloom,
            R.Manual (Stalk (R.Automatic (Seed.split next))) )

let to_string' : (string list * int * int) R.algebra =
 fun plant ->
  let mk_line ~len ~indent ~ch_fill ~ch =
    String.make indent ch_fill ^ ch ^ String.make (len - indent - 1) ch_fill
  in
  match plant with
  | Root (lines, len, indent) ->
      let line = mk_line ~len ~indent ~ch_fill:' ' ~ch:"^" in
      (line :: lines, len, indent)
  | Stalk (lines, len, indent) ->
      let line = mk_line ~len ~indent ~ch_fill:' ' ~ch:"|" in
      (line :: lines, len, indent)
  | Fork
      ((llines, llen, lindent), (clines, clen, cindent), (rlines, rlen, rindent))
    ->
      let top_filling ~nlines ~line_len =
        List.init nlines (String.make line_len ' ' |> Fun.const)
      in
      let fill_with_space_left lines = List.map (fun s -> " " ^ s) lines in
      let fill_with_space_right lines = List.map (fun s -> s ^ " ") lines in
      let nllines, nclines, nrlines =
        (List.length llines, List.length clines, List.length rlines)
      in
      let nlines = max (max nllines nclines) nrlines in
      let llines =
        llines @ top_filling ~nlines:(nlines - nllines) ~line_len:llen
        |> fill_with_space_right
      in
      let llen =
        llen + 1
        (* due to filling with space the llines above *)
      in
      let clines =
        clines @ top_filling ~nlines:(nlines - nclines) ~line_len:clen
      in
      let rlines =
        rlines @ top_filling ~nlines:(nlines - nrlines) ~line_len:rlen
        |> fill_with_space_left
      in
      let rlen =
        rlen + 1
        (* due to filling with space the rlines above *)
      in
      let len, indent = (llen + clen + rlen, llen + cindent) in
      let lines = List.map2 ( ^ ) llines (List.map2 ( ^ ) clines rlines) in
      let lline =
        String.make lindent ' ' ^ "+" ^ String.make (llen - lindent - 1) '-'
      in
      let rline =
        String.make (rindent + 1) '-'
        ^ "+"
        ^ String.make (rlen - rindent - 1 - 1) ' '
      in
      ((lline ^ "+" ^ rline) :: lines, len, indent)
  | Bloom -> ([ "*" ], 1, 0)

let to_string_list p =
  let lines, _, _ = R.cata to_string' p in
  lines

let to_string p =
  let lines = to_string_list p in
  String.concat "\n" (List.rev lines)

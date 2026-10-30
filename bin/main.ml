let () = print_endline (Deffect.print Typegist.Type.Gist.bool true)
let () = print_endline (Deffect.print Typegist.Type.Gist.(t2 bool utf_8_string) (true, "lol"))
(* issue: want to overwrite the Col* effects handlers, but can't access (out of scope) buffer to add stuff to
let () =
  match
  print_endline (Deffect.print Typegist.Type.Gist.(t2 bool Deffect.StringSet.gist) (true, (Deffect.StringSet.empty)))
  with
*)

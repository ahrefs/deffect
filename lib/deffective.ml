(*

module Compare = struct
  type _ Effect.t += Compare : 'a Typegist.Type.Gist.t * 'a * 'a -> bool Effect.t
  let compare g = fun a b -> Effect.perform (Compare (g, a, b))
  let effc : bool Effect.Deep.effect_handler = { effc = fun (type b) eff -> match eff with
    | Compare (g, a, b) -> Some (fun k ->
        failwith "TODO"; k false
        )
    | _ -> None
  }
end
*)

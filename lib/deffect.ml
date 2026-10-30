(*
  issue: effects carry the value with them: good for consuming/traversing existing value, useless for e.g. random gen a value, useless for consuming two values for e.g. comparison
  - need to double the effects? one for consuming, one for producing? also one for consuming two? three? do we sometimes need both consume and produce?

  issue: effects have to return a given type but we sometimes want to serialise to a string, sometimes hash into a int64, sometimes compare values, etc. so we need different return types
  - need to have effects per generic function we want? Compare effect and Print effect and Sample effect?

 *)



module Meta : sig
  type 'a t = 'a -> unit
  include Typegist.Type.Gist.Meta.KEY with type ('a, 'b) value = 'b t
end = struct
  type 'a t = 'a -> unit
  type ('a, 'b) value = 'b t
  module V = struct type nonrec ('a, 'b) t = 'b t end
  include Typegist.Type.Gist.Meta.Key (V)
end


type _ Effect.t += Unit : unit -> unit Effect.t
type _ Effect.t += Bool : bool -> unit Effect.t
type _ Effect.t += Char : char -> unit Effect.t
type _ Effect.t += Uchar : Uchar.t -> unit Effect.t
type _ Effect.t += Int : int -> unit Effect.t
type _ Effect.t += Int32 : int32 -> unit Effect.t
type _ Effect.t += Int64 : int64 -> unit Effect.t
type _ Effect.t += Nativeint : nativeint -> unit Effect.t
type _ Effect.t += Float : float -> unit Effect.t
type _ Effect.t += String : string -> unit Effect.t

let performance_scalar
: type a. a Typegist.Type.Gist.Scalar.t -> a -> unit
= fun s v ->
  match s with
  | Unit -> Effect.perform (Unit v)
  | Bool -> Effect.perform (Bool v)
  | Char -> Effect.perform (Char v)
  | Uchar -> Effect.perform (Uchar v)
  | Int -> Effect.perform (Int v)
  | Int32 -> Effect.perform (Int32 v)
  | Int64 -> Effect.perform (Int64 v)
  | Nativeint -> Effect.perform (Nativeint v)
  | Float -> Effect.perform (Float v)

type collectionKind = Tuple | Record | Collection of string

type _ Effect.t += ColStart : collectionKind -> unit Effect.t
type _ Effect.t += ColElt : collectionKind * Typegist.Type.Gist.Field.name * int -> unit Effect.t
type _ Effect.t += ColEnd : collectionKind -> unit Effect.t

let rec performance_fields
: type p v. collectionKind -> int -> (p, v) Typegist.Type.Gist.Product.fields -> p -> unit
= fun ck idx fields v ->
  match fields with
  | Ctor _ -> ()
  | App (fields, field) ->
      Effect.perform (ColElt (ck, Typegist.Type.Gist.Field.name field, idx));
      performance (Typegist.Type.Gist.Field.gist field) (Typegist.Type.Gist.Field.get field v);
      performance_fields ck (idx+1) fields v

and performance_product
: type a. collectionKind -> a Typegist.Type.Gist.Product.t -> a -> unit
= fun ck p v ->
  Effect.perform (ColStart ck);
  performance_fields ck 0 p v;
  Effect.perform (ColEnd ck)

and performance_tuple
: type a. a Typegist.Type.Gist.Tuple.t -> a -> unit
= fun p v ->
  performance_product Tuple p v

and performance_record
: type a. a Typegist.Type.Gist.Record.t -> a -> unit
= fun p v ->
  performance_product Record p v

and performance
: type a. a Typegist.Type.Gist.t -> a -> unit
= fun g v ->
  let meta = Typegist.Type.Gist.meta g in
  match Meta.find meta with
  | Some f -> f v
  | None ->
    match Typegist.Type.Gist.expr g with
    | Scalar s -> performance_scalar s v
    | Tuple p -> performance_tuple p v
    | Record r -> performance_record r v
    | Variant_like _ -> failwith "TDOD"
    | Array_like (String _) -> Effect.perform (String v)
    | Array_like _ -> failwith "TDOD"
    | Map_like _ -> failwith "TDOD"
    | Cell_like _ -> failwith "TDOD"
    | Func _ -> failwith "TDOD"
    | Abstract _ -> failwith "TODO"
    | View _ -> failwith "TDOD"
    | Rec _ -> failwith "TDOD"

module StringSet
: sig
    include Set.S with type elt = string
    val gist : t Typegist.Type.Gist.t
end
= struct
    include Set.Make(String)
    let gist =
      let base = Typegist.Type.Gist.abstract "StringSet.t" [] in
      let meta = Typegist.Type.Gist.meta base in
      let meta =
        Typegist.Type.Gist.Meta.add
          (module Meta)
          (fun s ->
            Effect.perform (ColStart (Collection "StringSet"));
            Seq.iteri (fun idx e -> Effect.perform (ColElt (Collection "StringSet", e, idx))) (to_seq s);
            Effect.perform (ColEnd (Collection "StringSet")))
          meta
      in
      Typegist.Type.Gist.update ~meta base
end

module StringSet2
: sig
    include Set.S with type elt = string
    type _ Effect.t += StringSet2 : t -> unit Effect.t
    val gist : t Typegist.Type.Gist.t
end
= struct
    include Set.Make(String)
    type _ Effect.t += StringSet2 : t -> unit Effect.t
    let gist =
      let base = Typegist.Type.Gist.abstract "StringSet2.t" [] in
      let meta = Typegist.Type.Gist.meta base in
      let meta =
        Typegist.Type.Gist.Meta.add
          (module Meta)
          (fun s -> Effect.perform (StringSet2 s))
          meta
      in
      Typegist.Type.Gist.update ~meta base
end

let print g v =
  let b = Buffer.create 64 in
  let rec loop () =
    match performance g v with
    | () -> Buffer.contents b
    | effect Bool true, k -> Buffer.add_string b "true"; Effect.Deep.continue k ()
    | effect Bool false, k -> Buffer.add_string b "true"; Effect.Deep.continue k ()
    | effect String s, k -> Buffer.add_string b (Printf.sprintf "%S" s); Effect.Deep.continue k ()
    | effect ColStart Tuple, k -> Buffer.add_string b "["; Effect.Deep.continue k ()
    | effect ColElt (Tuple, "", n), k -> if n <> 0 then Buffer.add_string b ","; Effect.Deep.continue k ()
    | effect ColEnd Tuple, k -> Buffer.add_string b "]"; Effect.Deep.continue k ()
    | effect ColStart Record, k -> Buffer.add_string b "{"; Effect.Deep.continue k ()
    | effect ColElt (Record, n, i), k ->
        if i <> 0 then Buffer.add_string b ",";
        Buffer.add_string b (Printf.sprintf "%S" n);
        Buffer.add_char b '=';
        Effect.Deep.continue k ()
    | effect ColEnd Record, k -> Buffer.add_string b "}"; Effect.Deep.continue k ()
  in
  loop ()


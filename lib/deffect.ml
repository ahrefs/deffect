
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
type _ Effect.t += ColElt : collectionKind * Typegist.Type.Gist.Field.name -> unit Effect.t
type _ Effect.t += ColEnd : collectionKind -> unit Effect.t

let rec performance_fields
: type p v. collectionKind -> (p, v) Typegist.Type.Gist.Product.fields -> p -> unit
= fun ck fields v ->
  match fields with
  | Ctor _ -> ()
  | App (fields, field) ->
      Effect.perform (ColElt (ck, Typegist.Type.Gist.Field.name field));
      performance (Typegist.Type.Gist.Field.gist field) (Typegist.Type.Gist.Field.get field v);
      performance_fields ck fields v

and performance_product
: type a. collectionKind -> a Typegist.Type.Gist.Product.t -> a -> unit
= fun ck p v ->
  Effect.perform (ColStart ck);
  performance_fields ck p v;
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
            iter (fun e -> Effect.perform (ColElt (Collection "StringSet", e))) s;
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

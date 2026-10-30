type name = string

type _ Effect.t += SeqStart : name -> unit Effect.t
type _ Effect.t += SeqElt : name * name -> unit Effect.t
type _ Effect.t += SeqEnd : name -> unit Effect.t

type _ Effect.t += MapStart : name -> unit Effect.t
type _ Effect.t += MapK : name * name -> unit Effect.t
type _ Effect.t += MapV : name * name -> unit Effect.t
type _ Effect.t += MapEnd : name -> unit Effect.t

type _ Effect.t += Unit : unit -> unit Effect.t
type _ Effect.t += Bool : bool -> unit Effect.t
type _ Effect.t += Char : char -> unit Effect.t
type _ Effect.t += Uchar : Uchar.t -> unit Effect.t
type _ Effect.t += Int : int -> unit Effect.t
type _ Effect.t += Int32 : int32 -> unit Effect.t
type _ Effect.t += Int64 : int64 -> unit Effect.t
type _ Effect.t += Nativeint : nativeint -> unit Effect.t
type _ Effect.t += Float : float -> unit Effect.t

type json = Yojson.Safe.t
val bin : string
val ( / ) : string -> string -> string
val env : (string * string) list
val merge_env : (string * string) list -> (string * string) list -> (string * string) list
val mkdir : string -> unit
val read : string -> string
val write : string -> string -> unit
val copy_tree : string -> string -> unit
val remove_tree : string -> unit
val with_temp_dir : ?prefix:string -> (string -> 'a) -> 'a
val pause : float -> unit
val until : ?seconds:float -> predicate:('a -> bool) -> (unit -> 'a) -> 'a
val wait : ?seconds:float -> (unit -> bool) -> unit
val check : bool -> string -> unit
val equal : ?label:string -> 'a -> 'a -> unit
val field : string -> json -> json
val s : string -> json -> string
val i : string -> json -> int
val xs : string -> json -> json list
val string : string -> json
val obj : (string * json) list -> json
val put : string -> json -> json -> json
val puts : (string * json) list -> json -> json
type result = {code:int; stdout:string; stderr:string}
type child
val spawn : ?cwd:string -> ?env:(string * string) list -> ?input:string -> ?timeout:float -> ?log:string -> ?new_session:bool -> string -> string list -> child
val await : ?timeout:float -> child -> result
val run : ?cwd:string -> ?env:(string * string) list -> ?input:string -> ?timeout:float -> ?new_session:bool -> string -> string list -> result
val terminate : child -> unit
val kill : child -> unit
val pid : child -> int
val poll : child -> int option
val sdk_python : string -> string
exception Http_error of int * json
val http_error : int -> (unit -> 'a) -> unit
module Client : sig
  type t = {home:string;url:string;token:string}
  val create : string -> t
  val req : ?data:json -> ?headers:(string * string) list -> t -> string -> json
  val raw : ?data:string -> ?headers:(string * string) list -> ?meth:Cohttp.Code.meth -> t -> string -> int * string
  val cmd : ?status:string -> t -> string -> json -> json
  val parallel_cmd : ?status:string -> t -> string -> json -> count:int -> json list
  val parallel_req : t -> (string * json) list -> json list
  val history : t -> string -> json
  val revision : t -> string -> json
end
module Service : sig
  type t = {home:string;config:string;mutable env:(string * string) list;mutable process:child option;mutable client_value:Client.t option}
  val create : ?extra:(string * string) list -> string -> string -> t
  val client : t -> Client.t
  val start : ?port:int -> t -> unit
  val stop : t -> unit
  val close : t -> unit
  val pid : t -> int
end
module Native = Fake_native

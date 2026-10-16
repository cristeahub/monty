type json = Yojson.Safe.t
module Gate : sig
  type t = {mutable started:bool; promise:unit Lwt.t; wake:unit Lwt.u}
  val create : unit -> t
  val release : t -> unit
end
type t = {
  path:string;
  threads:(string,json) Hashtbl.t;
  loaded:(string,unit) Hashtbl.t;
  mutable calls:(string * json) list;
  mutable answers:json list;
  mutable sends:int;
  mutable drop_next:bool;
  mutable drop_open:bool;
  mutable history_gate:Gate.t option;
  mutable clients:(Lwt_io.input_channel * Lwt_io.output_channel * Lwt_mutex.t) list;
  mutable order:string list;
  mutable server:Lwt_io.server option;
  mutable errors:exn list;
}
val create : string -> t
val close : t -> unit
val emit : t -> json -> unit
val set_thread : t -> string -> json -> unit

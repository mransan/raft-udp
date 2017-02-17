(** raft_srv.proto Generated Types and Encoding *)


(** {2 Types} *)

type log_interval_compacted = {
  record_id : string;
}

type log_interval_expanded = {
  entries : Raft_pb.log_entry list;
}

type log_interval_rev_log_entries =
  | Compacted of log_interval_compacted
  | Expanded of log_interval_expanded

and log_interval = {
  prev_index : int;
  prev_term : int;
  last_index : int;
  rev_log_entries : log_interval_rev_log_entries;
}


(** {2 Default values} *)

val default_log_interval_compacted : 
  ?record_id:string ->
  unit ->
  log_interval_compacted
(** [default_log_interval_compacted ()] is the default value for type [log_interval_compacted] *)

val default_log_interval_expanded : 
  ?entries:Raft_pb.log_entry list ->
  unit ->
  log_interval_expanded
(** [default_log_interval_expanded ()] is the default value for type [log_interval_expanded] *)

val default_log_interval_rev_log_entries : unit -> log_interval_rev_log_entries
(** [default_log_interval_rev_log_entries ()] is the default value for type [log_interval_rev_log_entries] *)

val default_log_interval : 
  ?prev_index:int ->
  ?prev_term:int ->
  ?last_index:int ->
  ?rev_log_entries:log_interval_rev_log_entries ->
  unit ->
  log_interval
(** [default_log_interval ()] is the default value for type [log_interval] *)


(** {2 Protobuf Decoding} *)

val decode_log_interval_compacted : Pbrt.Decoder.t -> log_interval_compacted
(** [decode_log_interval_compacted decoder] decodes a [log_interval_compacted] value from [decoder] *)

val decode_log_interval_expanded : Pbrt.Decoder.t -> log_interval_expanded
(** [decode_log_interval_expanded decoder] decodes a [log_interval_expanded] value from [decoder] *)

val decode_log_interval_rev_log_entries : Pbrt.Decoder.t -> log_interval_rev_log_entries
(** [decode_log_interval_rev_log_entries decoder] decodes a [log_interval_rev_log_entries] value from [decoder] *)

val decode_log_interval : Pbrt.Decoder.t -> log_interval
(** [decode_log_interval decoder] decodes a [log_interval] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_log_interval_compacted : log_interval_compacted -> Pbrt.Encoder.t -> unit
(** [encode_log_interval_compacted v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval_expanded : log_interval_expanded -> Pbrt.Encoder.t -> unit
(** [encode_log_interval_expanded v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval_rev_log_entries : log_interval_rev_log_entries -> Pbrt.Encoder.t -> unit
(** [encode_log_interval_rev_log_entries v encoder] encodes [v] with the given [encoder] *)

val encode_log_interval : log_interval -> Pbrt.Encoder.t -> unit
(** [encode_log_interval v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_log_interval_compacted : Format.formatter -> log_interval_compacted -> unit 
(** [pp_log_interval_compacted v] formats v *)

val pp_log_interval_expanded : Format.formatter -> log_interval_expanded -> unit 
(** [pp_log_interval_expanded v] formats v *)

val pp_log_interval_rev_log_entries : Format.formatter -> log_interval_rev_log_entries -> unit 
(** [pp_log_interval_rev_log_entries v] formats v *)

val pp_log_interval : Format.formatter -> log_interval -> unit 
(** [pp_log_interval v] formats v *)

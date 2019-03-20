(** Binding to the maxminddb library which parses the MMDB format commonly known as GeoIP2 *)

(** Reference to a MMBD file *)
type t

(** Thrown when an error is detected that is an internal error of the library, not a
    usage error. It is recommended not to handle this, instead report a bug report in
    the library *)
exception Binding_integrity_error of string

module Common_error : sig
  type t =
    [ `Corrupt_search_tree of string
    | `Io_error of string
    | `Out_of_memory of string
    | `Invalid_data of string ]
  [@@deriving show]
end

module Open_file_error : sig
  type t =
    [ `File_open_error of string
    | `Invalid_metadata of string
    | `Unknown_database_format of string
    | Common_error.t ]
  [@@deriving show]
end

module Lookup_ip_error : sig
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]
end

module Fetch_value_error : sig
  (* `Unexpected_data_type is new - I think `Unsupported_data_type is abused
     in one spot so I split it *)
  type t =
    [ `Unsupported_data_type of string
    | `Unexpected_data_type of string
    | Common_error.t ]
end

module Lookup_error : sig
  type t = [Fetch_value_error.t | Lookup_ip_error.t] [@@deriving show]
end

module Path = Types.Path

(** Open an MMDB file and return a reference to it *)
val open_file : Path.t -> (t, Open_file_error.t) result

module Ip = Types.Ip

module Ip_data : sig
  type t
end

val look_up_ip : t -> Ip.t -> (Ip_data.t, Lookup_ip_error.t) result

module Lookup_result : sig
  type 'a t = ('a option, Lookup_error.t) result
end

module Fetch_result : sig
  type 'a t = ('a option, Fetch_value_error.t) result
end

module type VALUE_TYPE = sig
  module Query : sig
    type t

    val of_string_list : string list -> t

    val to_string_list : t -> string list
  end

  module Answer : sig
    type t
  end

  val look_up : t -> Ip.t -> Query.t -> Answer.t Lookup_result.t

  val fetch : Ip_data.t -> Query.t -> Answer.t Fetch_result.t
end

type any_answer =
  | String of string
  | Float of float
  | Int of int
  | Bool of bool

module Any : VALUE_TYPE with type Answer.t = any_answer

module String : sig
  include VALUE_TYPE with type Answer.t = string

  (* usage: sth. like `Mmdb.String.(look_up mmdb ip country_code)` *)
  (* usage: sth. like `Mmdb.String.(fetch ip_data country_code)` *)
  val country_code : Query.t

  val region_code : Query.t
end

module Coordinates_ : sig
  include VALUE_TYPE with type Answer.t = Coordinates.t

  val coordinates : Query.t
end

module Float : VALUE_TYPE with type Answer.t = float

module Int : VALUE_TYPE with type Answer.t = int

module Bool : VALUE_TYPE with type Answer.t = bool

module Coordinates = Coordinates

(** Determine the coordinates of an IP *)
val coordinates : t -> Ip.t -> Coordinates.t Lookup_result.t

(** Determine the country code of an IP *)
val country_code : t -> Ip.t -> string Lookup_result.t

(** Determine the region an IP is in *)
val region_code : t -> Ip.t -> string Lookup_result.t

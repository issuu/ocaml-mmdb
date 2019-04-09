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

module Fetch_ip_data_error : sig
  type t =
    [ `Invalid_address_info
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]
end

module Fetch_value_error : sig
  type t =
    [ `Invalid_lookup_path of string
    | `Invalid_node_number of string
    | `Unsupported_data_type of string
    | `Unexpected_data_type of string
    | Common_error.t ]
  [@@deriving show]
end

module Fetch_error : sig
  type t = [Fetch_ip_data_error.t | Fetch_value_error.t] [@@deriving show]
end

module Path = Types.Path

(** Open an MMDB file and return a reference to it *)
val open_file : Path.t -> (t, Open_file_error.t) result

module Ip = Types.Ip

type ip_data

val fetch_ip_data : t -> Ip.t -> (ip_data, Fetch_ip_data_error.t) result

module type VALUE_TYPE = sig
  module Query : sig
    type t

    val of_string_list : string list -> t

    val to_string_list : t -> string list
  end

  type answer

  val from_db : t -> Ip.t -> Query.t -> (answer option, Fetch_error.t) result

  val from_ip_data :
    ip_data -> Query.t -> (answer option, Fetch_value_error.t) result
end

type any_value =
  | String of string
  | Float of float
  | Int of int
  | Bool of bool

module Any : VALUE_TYPE with type answer = any_value

module String : sig
  include VALUE_TYPE with type answer = string

  (** Query that determines the code of the country where the IP is located *)
  val country_code : Query.t

  (** Query that determines the code of the region where the IP is located *)
  val region_code : Query.t
end

module Float : VALUE_TYPE with type answer = float

module Int : VALUE_TYPE with type answer = int

module Bool : VALUE_TYPE with type answer = bool

module Coordinates : sig
  include module type of Coordinates

  include VALUE_TYPE with type answer = Coordinates.t

  (** Query that determines the coordinates of an IP *)
  val location : Query.t
end

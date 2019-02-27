open Core

type t

type path = [`Path of string]

type ip = [`Ip of string]

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
    [ `Invalid_lookup_path of string
    | `Lookup_path_does_not_match_data of string
    | `Invalid_node_number of string
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]
end

module Lookup_error : sig
  type t = [`Unsupported_data_type of string | Lookup_ip_error.t]
  [@@deriving show]
end

module Lookup_result : sig
  type 'a t = ('a option, Lookup_error.t) Result.t
end

module Coordinates : sig
  type t = {longitude: float; latitude: float} [@@deriving show]
end

val open_file : path -> (t, Open_file_error.t) Result.t

val coordinates : t -> ip -> Coordinates.t Lookup_result.t

val country_code : t -> ip -> string Lookup_result.t

val region_code : t -> ip -> string Lookup_result.t

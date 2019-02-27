open Core

type t = Mmdb_types.Mmdb.t

type path = [`Path of string]

type ip = [`Ip of string]

module Int_ptr = struct
  let allocate () : int Ctypes.ptr =
    let array = Ctypes.CArray.make Ctypes.int ~initial:0 1 in
    Ctypes.CArray.start array
end

module Char_ptr = struct
  let of_string string =
    let length = Int.succ @@ String.length string in
    let result = Ctypes.CArray.make Ctypes.char ~initial:'\x00' length in
    let set_character index character =
      Ctypes.CArray.set result index character
    in
    let f index _ character = set_character index character in
    let () = String.foldi string ~init:() ~f in
    Ctypes.CArray.start result

  let to_string char_ptr =
    let char_arr = Ctypes.CArray.from_ptr char_ptr 0 in
    let rec to_string buffer index =
      let char = Ctypes.CArray.unsafe_get char_arr index in
      if Char.(char = '\x00') then ()
      else (
        Buffer.add_char buffer char ;
        Int.succ index |> to_string buffer )
    in
    let initial_size = 64 in
    let buffer = Buffer.create initial_size in
    let () = to_string buffer 0 in
    Buffer.contents buffer

  let to_string_of_length char_ptr ~length =
    let char_arr = Ctypes.CArray.from_ptr char_ptr length in
    let buffer = Buffer.create length in
    for index = 0 to length - 1 do
      let char = Ctypes.CArray.get char_arr index in
      Buffer.add_char buffer char
    done ;
    Buffer.contents buffer
end

module Char_ptr_ptr = struct
  let of_string_list strings =
    let char_ptrs = List.map strings ~f:Char_ptr.of_string in
    let length = Int.succ @@ List.length strings in
    let array =
      Ctypes.(CArray.make (ptr char) ~initial:(from_voidp char null) length)
    in
    let () =
      List.iteri char_ptrs ~f:(fun index char_ptr ->
          Ctypes.CArray.set array index char_ptr )
    in
    Ctypes.CArray.start array
end

module Mmdb_ptr = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_s" in
    let size = Mmdb_ffi.Helpers.size_of_mmdb_s () |> Unsigned.Size_t.to_int in
    let alignment =
      Mmdb_ffi.Helpers.alignment_of_mmdb_s () |> Unsigned.Size_t.to_int
    in
    Ctypes.abstract ~name ~size ~alignment

  let allocate () : Mmdb_types.Mmdb.t =
    Ctypes.allocate_n t ~count:1 |> Ctypes.to_voidp
end

module Entry_data_ptr = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_entry_data_s" in
    let size =
      Mmdb_ffi.Helpers.size_of_mmdb_entry_data_s () |> Unsigned.Size_t.to_int
    in
    let alignment =
      Mmdb_ffi.Helpers.alignment_of_mmdb_entry_data_s ()
      |> Unsigned.Size_t.to_int
    in
    Ctypes.abstract ~name ~size ~alignment

  let allocate () : Mmdb_types.Entry_data.t =
    Ctypes.allocate_n t ~count:1 |> Ctypes.to_voidp
end

module Error_code = struct
  let to_message error_code =
    Mmdb_ffi.Core.strerror error_code |> Char_ptr.to_string
end

module Common_error = struct
  type t =
    [ `Corrupt_search_tree of string
    | `Io_error of string
    | `Out_of_memory of string
    | `Invalid_data of string ]
  [@@deriving show]

  let of_error_code error_code =
    let get_message () = Error_code.to_message error_code in
    Mmdb_types.Error_code.(
      if error_code = success then None
      else if error_code = corrupt_search_tree_error then
        Some (`Corrupt_search_tree (get_message ()))
      else if error_code = io_error then Some (`Io_error (get_message ()))
      else if error_code = out_of_memory_error then
        Some (`Out_of_memory (get_message ()))
      else if error_code = invalid_data_error then
        Some (`Invalid_data (get_message ()))
      else
        Int.to_string error_code
        |> String.( ^ ) "Unrecognized error code: "
        |> failwith)
end

module Open_file_error = struct
  type t =
    [ `File_open_error of string
    | `Invalid_metadata of string
    | `Unknown_database_format of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code error_code =
    let get_message () = Error_code.to_message error_code in
    Mmdb_types.Error_code.(
      if error_code = file_open_error then
        Some (`File_open_error (get_message ()))
      else if error_code = invalid_metadata_error then
        Some (`Invalid_metadata (get_message ()))
      else if error_code = unknown_database_format_error then
        Some (`Unknown_database_format (get_message ()))
      else Common_error.of_error_code error_code)
end

module Lookup_ip_error = struct
  type t =
    [ `Invalid_lookup_path of string
    | `Lookup_path_does_not_match_data of string
    | `Invalid_node_number of string
    | `Ipv6_lookup_in_ipv4_database of string
    | Common_error.t ]
  [@@deriving show]

  let of_error_code error_code =
    let get_message () = Error_code.to_message error_code in
    Mmdb_types.Error_code.(
      if error_code = invalid_lookup_path_error then
        Some (`Invalid_lookup_path (get_message ()))
      else if error_code = lookup_path_does_not_match_data_error then
        Some (`Lookup_path_does_not_match_data (get_message ()))
      else if error_code = invalid_node_number_error then
        Some (`Invalid_node_number (get_message ()))
      else if error_code = ipv6_lookup_in_ipv4_database_error then
        Some (`Ipv6_lookup_in_ipv4_database (get_message ()))
      else Common_error.of_error_code error_code)
end

let open_file (`Path path) =
  let path = Char_ptr.of_string path in
  let mmdb = Mmdb_ptr.allocate () in
  let result = Mmdb_ffi.Core.open_file path Mmdb_types.Mmdb_mode.mmap mmdb in
  match Open_file_error.of_error_code result with
  | None -> Ok mmdb
  | Some error -> Error error

let lookup_ip mmdb ~ip =
  let ip = Char_ptr.of_string ip in
  let error_code_ptr1 = Int_ptr.allocate () in
  let error_code_ptr2 = Int_ptr.allocate () in
  let lookup_result =
    Mmdb_ffi.Core.lookup_string mmdb ip error_code_ptr1 error_code_ptr2
  in
  let error_code1 = Ctypes.(!@error_code_ptr1) in
  let error_code2 = Ctypes.(!@error_code_ptr2) in
  match Lookup_ip_error.of_error_code error_code1 with
  | Some e -> Error e
  | None -> (
    match Lookup_ip_error.of_error_code error_code2 with
    | Some e -> Error e
    | None -> Ok lookup_result )

module Supported_data_type = struct
  type t =
    | Utf8_String
    | Double
    | Uint16
    | Uint32
    | Int32
    | Uint64
    | Boolean
    | Float

  let of_data_type_code code =
    let data_type = Unsigned.UInt32.to_int code in
    Mmdb_types.Entry_data_type.(
      if data_type = utf8_string then Some Utf8_String
      else if data_type = double then Some Double
      else if data_type = uint16 then Some Uint16
      else if data_type = uint32 then Some Uint32
      else if data_type = int32 then Some Int32
      else if data_type = uint64 then Some Uint64
      else if data_type = boolean then Some Boolean
      else if data_type = float then Some Float
      else None)
end

module Query_result = struct
  type t = [`String of string | `Float of float | `Int of int | `Bool of bool]

  let get_query_result data_type entry_data_ptr =
    let module T = Supported_data_type in
    Mmdb_ffi.Helpers.(
      match data_type with
      | T.Utf8_String ->
          let length =
            Mmdb_ffi.Helpers.get_entry_data_size entry_data_ptr
            |> Unsigned.UInt32.to_int
          in
          `String
            ( entry_data_ptr |> get_entry_data_utf8_string_value
            |> Char_ptr.to_string_of_length ~length )
      | T.Double -> `Float (entry_data_ptr |> get_entry_data_double_value)
      | T.Uint16 ->
          `Int
            ( entry_data_ptr |> get_entry_data_uint16_value
            |> Unsigned.UInt16.to_int )
      | T.Uint32 ->
          `Int
            ( entry_data_ptr |> get_entry_data_uint32_value
            |> Unsigned.UInt32.to_int )
      | T.Int32 ->
          `Int
            ( entry_data_ptr |> get_entry_data_int32_value
            |> Signed.Int32.to_int )
      | T.Uint64 ->
          `Int
            ( entry_data_ptr |> get_entry_data_uint64_value
            |> Unsigned.UInt64.to_int )
      | T.Boolean -> `Bool (entry_data_ptr |> get_entry_data_boolean_value)
      | T.Float -> `Float (entry_data_ptr |> get_entry_data_float_value))
end

module Lookup_error = struct
  type t = [`Unsupported_data_type of string | Lookup_ip_error.t]
  [@@deriving show]
end

let run_query ~lookup_result ~query :
    (Query_result.t option, Lookup_error.t) Result.t =
  match Ctypes.getf lookup_result Mmdb_types.Lookup_result.found_entry with
  | false -> Ok None
  | true -> (
      let entry =
        Ctypes.getf lookup_result Mmdb_types.Lookup_result.entry |> Ctypes.addr
      in
      let entry_data_ptr = Entry_data_ptr.allocate () in
      let query = Char_ptr_ptr.of_string_list query in
      let error_code = Mmdb_ffi.Core.aget_value entry entry_data_ptr query in
      match Lookup_ip_error.of_error_code error_code with
      | Some e -> Error e
      | None -> (
        match Mmdb_ffi.Helpers.get_entry_data_has_data entry_data_ptr with
        | false -> Ok None
        | true -> (
            let data_type_code =
              Mmdb_ffi.Helpers.get_entry_data_type entry_data_ptr
            in
            match Supported_data_type.of_data_type_code data_type_code with
            | Some data_type ->
                Ok
                  (Some
                     (Query_result.get_query_result data_type entry_data_ptr))
            | None ->
                Error
                  (`Unsupported_data_type
                    (Unsigned.UInt32.to_string data_type_code)) ) ) )

let run_string_query ~lookup_result ~query =
  Result.Let_syntax.(
    match%bind run_query ~lookup_result ~query with
    | None -> Ok None
    | Some (`String s) -> Ok (Some s)
    | Some _ ->
        Error
          (`Unsupported_data_type
            "String query returned an unexpected value type"))

let run_float_query ~lookup_result ~query =
  Result.Let_syntax.(
    match%bind run_query ~lookup_result ~query with
    | None -> Ok None
    | Some (`Float f) -> Ok (Some f)
    | Some _ ->
        Error
          (`Unsupported_data_type
            "Float query returned an unexpected value type"))

module Lookup_result = struct
  type 'a t = ('a option, Lookup_error.t) Result.t
end

module Coordinates = struct
  type t = {longitude: float; latitude: float} [@@deriving show]
end

let coordinates mmdb (`Ip ip) =
  Result.Let_syntax.(
    let%bind lookup_result = lookup_ip mmdb ~ip in
    let%bind latitude =
      run_float_query ~lookup_result ~query:["location"; "latitude"]
    in
    let%bind longitude =
      run_float_query ~lookup_result ~query:["location"; "longitude"]
    in
    Ok
      ( match Option.both latitude longitude with
      | None -> None
      | Some (latitude, longitude) -> Some Coordinates.{latitude; longitude} ))

let lookup_string mmdb ~ip ~query =
  Result.Let_syntax.(
    let%bind lookup_result = lookup_ip mmdb ~ip in
    run_string_query ~lookup_result ~query)

let country_code mmdb (`Ip ip) =
  lookup_string mmdb ~ip ~query:["country"; "iso_code"]

let region_code mmdb (`Ip ip) =
  lookup_string mmdb ~ip ~query:["subdivisions"; "0"; "iso_code"]

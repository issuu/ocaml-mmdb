open Base

type t = Mmdb_types.Mmdb.t

module Common_error = Errors.Common_error
module Open_file_error = Errors.Open_file_error
module Lookup_ip_error = Errors.Lookup_ip_error
module Lookup_error = Errors.Lookup_error
module Path = Types.Path
module Ip = Types.Ip

let open_file path =
  let path = path |> Path.to_string |> Pointers.Char_ptr.of_string in
  let mmdb = Pointers.Mmdb.allocate () in
  let result = Mmdb_ffi.Core.open_file path Mmdb_types.Mmdb_mode.mmap mmdb in
  match Errors.Open_file_error.of_error_code result with
  | None -> Caml.Gc.finalise (fun mmdb -> Mmdb_ffi.Core.close mmdb) mmdb; (Ok mmdb)
  | Some error -> Error error

let lookup_ip mmdb ~ip =
  let ip = Pointers.Char_ptr.of_string ip in
  let error_code_ptr1 = Pointers.Int_ptr.allocate () in
  let error_code_ptr2 = Pointers.Int_ptr.allocate () in
  let lookup_result =
    Mmdb_ffi.Core.lookup_string mmdb ip error_code_ptr1 error_code_ptr2
  in
  let error_code1 = Ctypes.(!@error_code_ptr1) in
  let error_code2 = Ctypes.(!@error_code_ptr2) in
  match Errors.Lookup_ip_error.of_error_code error_code1 with
  | Some e -> Error e
  | None -> (
    match Errors.Lookup_ip_error.of_error_code error_code2 with
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
      match data_type with
      | data_type when data_type = utf8_string -> Some Utf8_String
      | data_type when data_type = double -> Some Double
      | data_type when data_type = uint16 -> Some Uint16
      | data_type when data_type = uint32 -> Some Uint32
      | data_type when data_type = int32 -> Some Int32
      | data_type when data_type = uint64 -> Some Uint64
      | data_type when data_type = boolean -> Some Boolean
      | data_type when data_type = float -> Some Float
      | _ -> None)
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
            |> Pointers.Char_ptr.to_string_of_length length )
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

let run_query ~lookup_result ~query :
    (Query_result.t option, Errors.Lookup_error.t) Caml.result =
  match Ctypes.getf lookup_result Mmdb_types.Lookup_result.found_entry with
  | false -> Ok None
  | true -> (
      let entry =
        Ctypes.getf lookup_result Mmdb_types.Lookup_result.entry |> Ctypes.addr
      in
      let entry_data_ptr = Pointers.Entry_data.allocate () in
      let query = Pointers.Char_ptr_ptr.of_string_list query in
      let error_code = Mmdb_ffi.Core.aget_value entry entry_data_ptr query in
      match Errors.Lookup_ip_error.of_error_code error_code with
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
  type 'a t = ('a option, Errors.Lookup_error.t) Result.t
end

let coordinates mmdb ip =
  Result.Let_syntax.(
    let ip = Ip.to_string ip in
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
    let ip = Ip.to_string ip in
    let%bind lookup_result = lookup_ip mmdb ~ip in
    run_string_query ~lookup_result ~query)

let country_code mmdb ip = lookup_string mmdb ~ip ~query:["country"; "iso_code"]

let region_code mmdb ip =
  lookup_string mmdb ~ip ~query:["subdivisions"; "0"; "iso_code"]

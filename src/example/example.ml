open Base

(* FIXME msl perhaps move these to a lower abstraction layer *)
(* FIXME msl these could be made more type-safe by using tags or some such *)
module Mmdb = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_s" in
    let size = Mmdb_ffi.Helpers.size_of_mmdb_s () in
    let alignment = Mmdb_ffi.Helpers.alignment_of_mmdb_s () in
    Ctypes.abstract ~name ~size ~alignment

  let allocate () : Mmdb_types.Mmdb.t =
    Ctypes.to_voidp @@ Ctypes.allocate_n t ~count:1
end

module EntryData = struct
  let t : unit Ctypes.abstract Ctypes.typ =
    let name = "mmdb_entry_data_s" in
    let size = Mmdb_ffi.Helpers.size_of_mmdb_entry_data_s () in
    let alignment = Mmdb_ffi.Helpers.alignment_of_mmdb_entry_data_s () in
    Ctypes.abstract ~name ~size ~alignment

  let allocate () : unit Ctypes.abstract Ctypes.ptr =
    Ctypes.allocate_n t ~count:1
end

type coordinates = {longitude: float; latitude: float}

type geolocation =
  { country_code: string option
  ; region: string option
  ; coordinates: coordinates option }

(** Result of a query, mostly relevant if you are doing it by hand
    with calls to lookup_path *)

(* type query_r =
  [`String of string | `Int of int | `Float of float | `Bool of bool] *)

module String = struct
  include String

  let iter : String.t -> f:(int -> char -> unit) -> unit =
   fun string ~f ->
    let f index _ character = f index character in
    String.foldi ~init:() string ~f
end

let char_ptr_of_string string =
  let length = Int.succ @@ String.length string in
  let result = Ctypes.CArray.make Ctypes.char ~initial:'\x00' length in
  let set_character index character =
    Ctypes.CArray.set result index character
  in
  let () = String.iter string ~f:set_character in
  Ctypes.CArray.start result

let int_ptr () =
  let result = Ctypes.CArray.make Ctypes.int ~initial:0 1 in
  Ctypes.CArray.start result

let char_ptr_ptr_of_string_list strings =
  let char_ptrs = List.map strings ~f:char_ptr_of_string in
  let length = Int.succ @@ List.length strings in
  let result =
    Ctypes.CArray.make (Ctypes.ptr Ctypes.char)
      ~initial:(Ctypes.from_voidp Ctypes.char Ctypes.null)
      length
  in
  let () =
    List.iteri char_ptrs ~f:(fun index char_ptr ->
        Ctypes.CArray.set result index char_ptr )
  in
  Ctypes.CArray.start result

(* let find_length char_ptr =
    let rec find_length char_ptr index =
        if is_char_ptr_end char_ptr index
        then index
        else Int.succ index |> find_length char_ptr 
    in find_length char_ptr 0 *)

let string_of_char_ptr (char_ptr : char Ctypes.ptr) =
  let char_arr = Ctypes.CArray.from_ptr char_ptr 0 in
  let is_char_ptr_end index =
    Char.(Ctypes.CArray.unsafe_get char_arr index = '\x00')
  in
  let rec string_of_char_ptr buffer index =
    if is_char_ptr_end index then ()
    else
      let char = Ctypes.CArray.unsafe_get char_arr index in
      Buffer.add_char buffer char ;
      Int.succ index |> string_of_char_ptr buffer
  in
  let buffer = Buffer.create 64 in
  let () = string_of_char_ptr buffer 0 in
  Buffer.contents buffer

let handle_error_code error_code =
  Stdio.print_endline @@ "error code: " ^ Int.to_string error_code ;
  if error_code = Mmdb_types.ErrorCode.success then Ok ()
  else
    Error
      ( Mmdb_ffi.Core.strerror error_code
      |> string_of_char_ptr |> Error.of_string )

let handle_error_codes ~value error_codes =
  let errors = List.map error_codes ~f:handle_error_code in
  Or_error.(combine_errors_unit errors >>= fun () -> return value)

(** Create a handle on a mmdb descriptor based off a .mmdb database
    file. Maxminddb comes with City, Country databases in etc *)
let create : path:string -> Mmdb_types.Mmdb.t Or_error.t =
 fun ~path ->
  let path = char_ptr_of_string path in
  let mmdb = Mmdb.allocate () in
  let result = Mmdb_ffi.Core.open_file path Mmdb_types.MmdbMode.mmap mmdb in
  handle_error_codes ~value:mmdb [result]

let fetch_geolocation :
    ip:string -> Mmdb_types.Mmdb.t -> geolocation Or_error.t =
 fun ~ip mmdb ->
  let ip = char_ptr_of_string ip in
  let err1 = int_ptr () in
  let err2 = int_ptr () in
  let r = Mmdb_ffi.Core.lookup_string mmdb ip err1 err2 in
  match handle_error_codes ~value:r Ctypes.[!@err1; !@err2] with
  | Error e -> Error e
  | Ok r ->
      Stdio.print_endline
      @@ Bool.to_string (Ctypes.getf r Mmdb_types.LookupResult.found_entry) ;
      Stdio.print_endline @@ Int.to_string Ctypes.(!@err1) ;
      Stdio.print_endline @@ Int.to_string Ctypes.(!@err2) ;
      let d = Ctypes.to_voidp @@ EntryData.allocate () in
      let q = char_ptr_ptr_of_string_list ["location"; "latitude"] in
      let rr =
        Mmdb_ffi.Core.aget_value
          (Ctypes.addr (Ctypes.getf r Mmdb_types.LookupResult.entry))
          d q
      in
      Stdio.print_endline @@ Int.to_string rr ;
      Mmdb_ffi.Helpers.examine d ;
      Ok {country_code= None; region= None; coordinates= None}

let () =
  let ip = "172.56.31.240" in
  match create ~path:"etc/GeoLite2-City.mmdb" with
  | Error e -> Stdio.print_endline @@ Error.to_string_hum e
  | Ok mmdb -> (
    match fetch_geolocation ~ip mmdb with
    | Error e -> Stdio.print_endline @@ Error.to_string_hum e
    | Ok _ -> () )

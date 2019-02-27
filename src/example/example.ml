open Base

(* FIXME msl turn this into a unit test *)

let () =
  let ip = "172.56.31.240" in
  match Mmdb.open_file (`Path "etc/GeoLite2-City.mmdb") with
  | Error e -> Mmdb.Open_file_error.show e |> Stdio.print_endline
  | Ok mmdb -> (
      ( match Mmdb.coordinates mmdb (`Ip ip) with
      | Error e -> Mmdb.Lookup_error.show e |> Stdio.print_endline
      | Ok (Some {latitude; longitude}) ->
          Stdio.print_endline @@ "latitude: " ^ Float.to_string latitude
          ^ "; longitude: " ^ Float.to_string longitude
      | Ok None -> Stdio.print_endline "No value there" ) ;
      ( match Mmdb.country_code mmdb (`Ip ip) with
      | Error e -> Mmdb.Lookup_error.show e |> Stdio.print_endline
      | Ok (Some country_code) ->
          Stdio.print_endline ("Country code " ^ country_code)
      | Ok None -> Stdio.print_endline "No value there" ) ;
      match Mmdb.region_code mmdb (`Ip ip) with
      | Error e -> Mmdb.Lookup_error.show e |> Stdio.print_endline
      | Ok (Some region_code) ->
          Stdio.print_endline ("Region code " ^ region_code)
      | Ok None -> Stdio.print_endline "No value there" )

open Base

module type ParametrizedTestSuite = sig
  type parameter

  val tests : (parameter * string) list

  val run_test : parameter -> unit
end

module ToAlcotestSuite (S : ParametrizedTestSuite) = struct
  let tests =
    List.map S.tests ~f:(fun (parameter, description) ->
        Alcotest.test_case description `Quick (fun () -> S.run_test parameter)
    )
end

module Path = struct
  let correct = Mmdb.Path.of_string "../../etc/GeoLite2-City.mmdb"

  let incorrect = Mmdb.Path.of_string "GeoGeoGeoGeoGeo.mmdb"
end

module Ip = struct
  let correct = Mmdb.Ip.of_string "172.56.31.240"

  let incorrect = Mmdb.Ip.of_string "172.56.31.240 blah"
end

module OpenFileSuite = ToAlcotestSuite (struct
  type expectation = Success | File_open_error

  type parameter = Mmdb.Path.t * expectation

  let test path expectation description = ((path, expectation), description)

  let tests =
    [ test Path.correct Success "Successfully opens a valid MMDB file"
    ; test Path.incorrect File_open_error
        "Returns 'File_open_error' when opening a non-existing MMDB file" ]

  let fail expected_desc actual_desc =
    Printf.sprintf "Expected %s but got %s" expected_desc actual_desc
    |> Alcotest.fail

  let run_test (path, expectation) =
    let actual = Mmdb.open_file path in
    match (expectation, actual) with
    | Success, Ok _ -> ()
    | Success, Error e -> Mmdb.Open_file_error.show e |> fail "an MMDB handle"
    | File_open_error, Ok _ ->
        fail "File_open_error" "an MMDB handle" |> Alcotest.fail
    | File_open_error, Error (`File_open_error _) -> ()
    | File_open_error, Error e ->
        Mmdb.Open_file_error.show e |> fail "File_open_error"
end)

module FetchingCommon = struct
  type expectation = Success | Invalid_lookup_path

  type parameter = Mmdb.Ip.t * expectation

  let test path expectation description = ((path, expectation), description)

  let tests fetch_name =
    [ Printf.sprintf "Can fetch %s for a valid IP address" fetch_name
      |> test Ip.correct Success
    ; Printf.sprintf
        "Returns 'Invalid_lookup_path' when fetching %s for an invalid IP \
         address"
        fetch_name
      |> test Ip.incorrect Invalid_lookup_path ]

  let fail expected_desc actual_desc =
    Printf.sprintf "Expected %s but got %s" expected_desc actual_desc
    |> Alcotest.fail

  let run_test fetch_name fetch (ip, expectation) =
    match Mmdb.open_file Path.correct with
    | Error e ->
        Mmdb.Open_file_error.show e |> fail "to open a valid MMDB file"
    | Ok mmdb -> (
        let actual = fetch mmdb ip in
        let fail_expect_value =
          Printf.sprintf "a %s value" fetch_name |> fail
        in
        let fail_expect_error =
          Printf.sprintf "an 'Invalid_lookup_path' error while looking up %s"
            fetch_name
          |> fail
        in
        match (expectation, actual) with
        | Success, Ok None -> "no value" |> fail_expect_value
        | Success, Ok (Some _) -> ()
        | Success, Error e -> Mmdb.Lookup_error.show e |> fail_expect_value
        | Invalid_lookup_path, Ok None -> "no value" |> fail_expect_error
        | Invalid_lookup_path, Ok (Some _) -> "a value" |> fail_expect_error
        | Invalid_lookup_path, Error (`Invalid_lookup_path _) -> ()
        | Invalid_lookup_path, Error e ->
            Mmdb.Lookup_error.show e |> fail_expect_error )
end

module CoordinateFetchingSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "coordinates"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.coordinates
end)

module CountryCodeFetchingSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "country code"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.country_code
end)

module RegionCodeFetchinSuite = ToAlcotestSuite (struct
  include FetchingCommon

  let fetch_name = "region code"

  let tests = tests fetch_name

  let run_test = run_test fetch_name Mmdb.region_code
end)

let () =
  let e2e_tests =
    List.concat
      [ OpenFileSuite.tests
      ; CoordinateFetchingSuite.tests
      ; CountryCodeFetchingSuite.tests
      ; RegionCodeFetchinSuite.tests ]
  in
  Alcotest.run "ocaml-mmdb test suite" [("End-to-end", e2e_tests)]

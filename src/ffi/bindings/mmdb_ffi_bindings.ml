module T = Mmdb_types

module M (F : Ctypes.FOREIGN) = struct
  module C = struct
    include Ctypes

    let ( @-> ) = F.( @-> )

    let returning = F.returning
  end

  module Core = struct
    let open_file =
      F.foreign "MMDB_open" C.(ptr char @-> int @-> T.Mmdb.t @-> returning int)

    let lookup_string =
      F.foreign "MMDB_lookup_string"
        C.(
          T.Mmdb.t @-> ptr char @-> ptr int @-> ptr int
          @-> returning T.LookupResult.t)

    let aget_value =
      F.foreign "MMDB_aget_value"
        C.(
          ptr T.Entry.t @-> T.EntryData.t @-> ptr (ptr char) @-> returning int)

    let strerror = F.foreign "MMDB_strerror" C.(int @-> returning (ptr char))
  end

  module Helpers = struct
    let size_of_mmdb_s =
      F.foreign "mmdb_ml_sizeof_mmdb_s" C.(void @-> returning int)

    let alignment_of_mmdb_s =
      F.foreign "mmdb_ml_alignof_mmdb_s" C.(void @-> returning int)

    let size_of_mmdb_entry_data_s =
      F.foreign "mmdb_ml_sizeof_mmdb_entry_data_s" C.(void @-> returning int)

    let alignment_of_mmdb_entry_data_s =
      F.foreign "mmdb_ml_alignof_mmdb_entry_data_s" C.(void @-> returning int)

    let examine = F.foreign "examine" C.(ptr void @-> returning void)
  end
end

#include <stdalign.h>
#include <maxminddb.h>

// FIXME msl perhaps rename some of these
size_t mmdb_ml_sizeof_mmdb_s(void);
size_t mmdb_ml_alignof_mmdb_s(void);
size_t mmdb_ml_sizeof_mmdb_entry_data_s(void);
size_t mmdb_ml_alignof_mmdb_entry_data_s(void);
void examine(MMDB_entry_data_s* data);

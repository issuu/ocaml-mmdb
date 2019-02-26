#include <stddef.h>
#include <maxminddb.h>

size_t mmdb_ml_sizeof_mmdb_s(void)
{
    return sizeof(MMDB_s);
}

size_t mmdb_ml_alignof_mmdb_s(void)
{
    return offsetof(struct { char c; struct MMDB_s x; }, x);
}

size_t mmdb_ml_sizeof_mmdb_entry_data_s(void)
{
    return sizeof(MMDB_entry_data_s);
}

size_t mmdb_ml_alignof_mmdb_entry_data_s(void)
{
    return offsetof(struct { char c; struct MMDB_entry_data_s x; }, x);
}

void examine(MMDB_entry_data_s* data)
{
    printf("type is %d\n", data->type);
    if (data->type == MMDB_DATA_TYPE_DOUBLE)
    {
        printf("value is %f\n", data->double_value);
    }
}

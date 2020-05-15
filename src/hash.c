#include <hash.h>

fu_size_t hash_bytes(fu_uint8_t *bytes, fu_size_t len) {
    if (len == 0) {
        return 0;
    }
    fu_size_t hash = 5381;
    fu_size_t i;
    for (i = 0; i < len; i++) {
        fu_uint8_t n = bytes[i];
        hash = ((hash << 5) + hash) + n;
    }
    return hash;
}

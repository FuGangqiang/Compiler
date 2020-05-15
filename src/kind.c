#include "parse.h"

char *FuKind_token_cstr(fu_token_k kd) {
    switch (kd) {
/* clang-format off */
#define TOKEN(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "token.def"
#undef TOKEN
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_keyword_cstr(fu_token_k kd) {
    switch (kd) {
/* clang-format off */
#define KEYWORD(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "keyword.def"
#undef KEYWORD
    default:
        /* can not be here */
        return "";
        break;
    }
}

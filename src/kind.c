#include "parse.h"

char *FuKind_arm_cstr(fu_arm_k kd) {
    switch (kd) {
/* clang-format off */
#define ARM(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_arm.def"
#undef ARM
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_attr_cstr(fu_attr_k kd) {
    switch (kd) {
/* clang-format off */
#define ATTR(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_attr.def"
#undef ATTR
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_expr_cstr(fu_expr_k kd) {
    switch (kd) {
/* clang-format off */
#define EXPR(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_expr.def"
#undef EXPR
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_ge_arg_cstr(fu_ge_arg_k kd) {
    switch (kd) {
/* clang-format off */
#define GE_ARG(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_ge_arg.def"
#undef GE_ARG
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_ge_param_cstr(fu_ge_param_k kd) {
    switch (kd) {
/* clang-format off */
#define GE_PARAM(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_ge_param.def"
#undef GE_PARAM
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_lit_cstr(fu_lit_k kd) {
    switch (kd) {
/* clang-format off */
#define LIT(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_lit.def"
#undef LIT
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_node_cstr(fu_node_k kd) {
    switch (kd) {
/* clang-format off */
#define NODE(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node.def"
#undef NODE
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_op_cstr(fu_op_k kd) {
    switch (kd) {
/* clang-format off */
#define OP(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_op.def"
#undef OP
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_pat_cstr(fu_pat_k kd) {
    switch (kd) {
/* clang-format off */
#define PAT(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_pat.def"
#undef PAT
    default:
        /* can not be here */
        return "";
        break;
    }
}

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

char *FuKind_keyword_cstr(fu_keyword_k kd) {
    switch (kd) {
/* clang-format off */
#define TYPE(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "type.def"
#undef TYPE
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_use_cstr(fu_use_k kd) {
    switch (kd) {
/* clang-format off */
#define USE(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_use.def"
#undef USE
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_variant_cstr(fu_variant_k kd) {
    switch (kd) {
/* clang-format off */
#define VARIANT(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_variant.def"
#undef VARIANT
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_vis_cstr(fu_vis_k kd) {
    switch (kd) {
/* clang-format off */
#define VIS(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "node_vis.def"
#undef VIS
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_type_cstr(fu_type_k kd) {
    switch (kd) {
/* clang-format off */
#define TYPE(kd, comment) \
    case kd: \
        return #kd; \
        break;
/* clang-format on */
#include "type.def"
#undef TYPE
    default:
        /* can not be here */
        return "";
        break;
    }
}

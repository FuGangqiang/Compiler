#include "log.h"
#include "parse.h"

char *FuKind_log_cstr(fu_log_k kd) {
    switch (kd) {
/* clang-format off */
#define LOG(kd, doc) \
    case kd:         \
        return doc;  \
        break;
/* clang-format on */
#include "log.def"
#undef LOG
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_arm_cstr(fu_arm_k kd) {
    switch (kd) {
/* clang-format off */
#define ARM(kd, _doc) \
    case kd:          \
        return #kd;   \
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
#define ATTR(kd, _doc) \
    case kd:           \
        return #kd;    \
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
#define EXPR(kd, _doc) \
    case kd:           \
        return #kd;    \
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

char *FuKind_field_cstr(fu_field_k kd) {
    switch (kd) {
/* clang-format off */
#define FIELD(kd, _doc) \
    case kd:            \
        return #kd;     \
        break;
/* clang-format on */
#include "node_field.def"
#undef FIELD
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_ge_arg_cstr(fu_ge_arg_k kd) {
    switch (kd) {
/* clang-format off */
#define GE_ARG(kd, _doc) \
    case kd:             \
        return #kd;      \
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
#define GE_PARAM(kd, _doc) \
    case kd:               \
        return #kd;        \
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
#define LIT(kd, _doc) \
    case kd:          \
        return #kd;   \
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
#define NODE(kd, _doc) \
    case kd:           \
        return #kd;    \
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
#define OP(kd, _prec, _assoc, _ty, _doc) \
    case kd:                             \
        return #kd;                      \
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

fu_op_prec_t FuOp_precedence(fu_op_k kd) {
    switch (kd) {
/* clang-format off */
#define OP(kd, prec, _assoc, _ty, _doc) \
    case kd:                            \
        return prec;                    \
        break;
/* clang-format on */
#include "node_op.def"
#undef OP
    default:
        FATAL(NULL, "can not be here");
        return 0;
        break;
    }
}

fu_bool_t FuOp_is_unary(fu_op_k kd) {
    switch (kd) {
    case OP_DEREF:
    case OP_NOT:
    case OP_NEG:
    case OP_ADDRESS:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuOp_is_binary(fu_op_k kd) {
    switch (kd) {
    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
    case OP_REM:
    case OP_BIT_AND:
    case OP_BIT_XOR:
    case OP_BIT_OR:
    case OP_SHL:
    case OP_SHR:
    case OP_LT:
    case OP_LE:
    case OP_GT:
    case OP_GE:
    case OP_EQ:
    case OP_NE:
    case OP_AND:
    case OP_OR:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_op_prec_t FuTyOp_precedence(fu_ty_op_k kd) {
    switch (kd) {
/* clang-format off */
#define TYPE_OP(kd, prec, _assoc, _ty, _doc) \
    case kd:                                 \
        return prec;                         \
        break;
/* clang-format on */
#include "type_op.def"
#undef TYPE_OP
    default:
        FATAL(NULL, "can not be here");
        return 0;
        break;
    }
}

char *FuKind_pat_cstr(fu_pat_k kd) {
    switch (kd) {
/* clang-format off */
#define PAT(kd, _doc) \
    case kd:          \
        return #kd;   \
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
#define TOKEN(kd, _doc) \
    case kd:            \
        return #kd;     \
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
#define KEYWORD(kd, _doc) \
    case kd:           \
        return #kd;    \
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

char *FuKind_use_cstr(fu_use_k kd) {
    switch (kd) {
/* clang-format off */
#define USE(kd, _doc) \
    case kd:          \
        return #kd;   \
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
#define VARIANT(kd, _doc) \
    case kd:              \
        return #kd;       \
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
#define VIS(kd, _doc) \
    case kd:          \
        return #kd;   \
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

char *FuKind_ty_op_cstr(fu_ty_op_k kd) {
    switch (kd) {
/* clang-format off */
#define TYPE_OP(kd, _prec, _assoc, _ty, _doc) \
    case kd:                                  \
        return #kd;                           \
        break;
/* clang-format on */
#include "type_op.def"
#undef TYPE_OP
    default:
        /* can not be here */
        return "";
        break;
    }
}

char *FuKind_type_cstr(fu_type_k kd) {
    switch (kd) {
/* clang-format off */
#define TYPE(kd, doc) \
    case kd:           \
        return doc;    \
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

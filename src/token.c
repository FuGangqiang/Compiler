#include <assert.h>

#include "log.h"
#include "parse.h"

FuToken FuToken_new(fu_token_k kd, FuSpan *sp) {
    assert(kd < TOK_DOC_COMMENT);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    return token;
}

static FuToken FuToken_new_sym(fu_token_k kd, FuSpan *sp, fu_sym_t sym) {
    assert(kd >= TOK_DOC_COMMENT && kd < TOK_BYTE);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    token.sym = sym;
    return token;
}

FuToken FuToken_new_doc_comment(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_DOC_COMMENT, sp, sym);
}

FuToken FuToken_new_keyword(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_KEYWORD, sp, sym);
}

FuToken FuToken_new_ident(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_IDENT, sp, sym);
}

FuToken FuToken_new_raw_ident(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_RAW_IDENT, sp, sym);
}

FuToken FuToken_new_macro(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_MACRO, sp, sym);
}

FuToken FuToken_new_lable(FuSpan *sp, fu_sym_t sym) {
    return FuToken_new_sym(TOK_LABEL, sp, sym);
}

FuToken FuToken_new_lit_int(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_int, fu_size_t suffix_start) {
    FuToken token;
    token.kd = TOK_INT;
    token.sp = sp;
    token._int.sym = sym;
    token._int.base = base;
    token._int.empty_int = empty_int;
    token._int.suffix_start = suffix_start;
    return token;
}

FuToken FuToken_new_lit_float(FuSpan *sp, fu_sym_t sym, fu_size_t base, fu_bool_t empty_exponent,
                              fu_size_t suffix_start) {
    FuToken token;
    token.kd = TOK_FLOAT;
    token.sp = sp;
    token._float.sym = sym;
    token._float.base = base;
    token._float.empty_exponent = empty_exponent;
    token._float.suffix_start = suffix_start;
    return token;
}

FuToken FuToken_new_lit_char(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated) {
    FuToken token;
    token.kd = TOK_CHAR;
    token.sp = sp;
    token._char.sym = sym;
    token._char.terminated = terminated;
    return token;
}

FuToken FuToken_new_lit_byte(FuSpan *sp, fu_sym_t sym, fu_bool_t terminated) {
    FuToken token;
    token.kd = TOK_BYTE;
    token.sp = sp;
    token._byte.sym = sym;
    token._byte.terminated = terminated;
    return token;
}

FuToken FuToken_new_lit_str(fu_token_k kd, FuSpan *sp, fu_sym_t sym, fu_size_t n_hashes, fu_bool_t started,
                            fu_size_t prefix_ignore, fu_bool_t terminated) {
    assert(TOK_STR <= kd && kd <= TOK_FORMAT_RAW_STR);
    FuToken token;
    token.kd = kd;
    token.sp = sp;
    token._str.sym = sym;
    token._str.n_hashes = n_hashes;
    token._str.started = started;
    token._str.prefix_ignore = prefix_ignore;
    token._str.terminated = terminated;
    return token;
}

fu_bool_t FuToken_is_eof(FuToken tok) {
    return tok.kd == TOK_EOF;
}

fu_bool_t FuToken_is_ident(FuToken tok) {
    if (tok.kd == TOK_RAW_IDENT) {
        return FU_TRUE;
    }
    if (tok.kd == TOK_IDENT && tok.sym >= _KW_LAST_UNUSED) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_keyword(FuToken tok) {
    if (tok.kd == TOK_IDENT && tok.sym < _KW_LAST_UNUSED) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_open_delim(FuToken tok) {
    switch (tok.kd) {
    case TOK_OPEN_PAREN:
    case TOK_OPEN_BRACKET:
    case TOK_OPEN_BRACE:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_close_delim(FuToken tok) {
    switch (tok.kd) {
    case TOK_CLOSE_PAREN:
    case TOK_CLOSE_BRACKET:
    case TOK_CLOSE_BRACE:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_match_delim(FuToken open, FuToken close) {
    if (open.kd == TOK_OPEN_PAREN && close.kd == TOK_CLOSE_PAREN) {
        return FU_TRUE;
    }
    if (open.kd == TOK_OPEN_BRACKET && close.kd == TOK_CLOSE_BRACKET) {
        return FU_TRUE;
    }
    if (open.kd == TOK_OPEN_BRACE && close.kd == TOK_CLOSE_BRACE) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_bin_op(FuToken tok) {
    switch (tok.kd) {
    case TOK_PLUS:
    case TOK_MINUS:
    case TOK_STAR:
    case TOK_SLASH:
    case TOK_PERCENT:
    case TOK_AND:
    case TOK_OR:
    case TOK_CARET:
    case TOK_SHL:
    case TOK_SHR:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_bin_eq_op(FuToken tok) {
    switch (tok.kd) {
    case TOK_PLUS_EQ:
    case TOK_MINUS_EQ:
    case TOK_STAR_EQ:
    case TOK_SLASH_EQ:
    case TOK_PERCENT_EQ:
    case TOK_AND_EQ:
    case TOK_OR_EQ:
    case TOK_CARET_EQ:
    case TOK_SHL_EQ:
    case TOK_SHR_EQ:
        return FU_TRUE;
        break;
    default:
        return FU_FALSE;
    }
}

fu_bool_t FuToken_is_lit(FuToken tok) {
    if (TOK_BYTE <= tok.kd && tok.kd <= TOK_FORMAT_RAW_STR) {
        return FU_TRUE;
    }
    if (tok.kd == TOK_KEYWORD) {
        /* ident: nil, true, false */
        if (tok.sym == KW_NIL || tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
            return FU_TRUE;
        }
    }
    return FU_FALSE;
}

fu_bool_t FuToken_is_outer_doc_comment(FuToken tok) {
    assert(tok.kd == TOK_DOC_COMMENT);
    FuStr *symbol = FuCtx_get_symbol(tok.sp->ctx, tok.sym);
    if (FuStr_get_char(symbol, 0) == '/') {
        return FU_TRUE;
    }
    if (FuStr_get_char(symbol, 0) == '!') {
        return FU_FALSE;
    }
    FATAL(NULL, "can not be here");
    return 0;
}

fu_bool_t FuToken_is_blank(FuToken tok) {
    if (tok.kd == TOK_NEWLINE) {
        return FU_TRUE;
    }
    if (tok.kd == TOK_WHITESPACE) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

/* clang-format off */
#define BIND(tok_kind, op_kind) \
    case tok_kind: {            \
        if (op) {               \
            *op = op_kind;      \
            return FU_TRUE;     \
        }                       \
        break;                  \
    }
/* clang-format on */

fu_bool_t FuToken_to_prefix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_STAR, OP_DEREF)
        BIND(TOK_NOT, OP_NOT)
        BIND(TOK_MINUS, OP_NEG)
        BIND(TOK_AND, OP_ADDRESS)
        BIND(TOK_DOT_DOT, OP_RANGE)
        BIND(TOK_DOT_DOT_EQ, OP_RANGE_EQ)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_infix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
    case TOK_KEYWORD: {
        if (tok.sym == KW_AS) {
            *op = OP_CAST;
            return FU_TRUE;
        }
        break;
    }
        BIND(TOK_PLUS, OP_ADD)
        BIND(TOK_MINUS, OP_SUB)
        BIND(TOK_STAR, OP_MUL)
        BIND(TOK_SLASH, OP_DIV)
        BIND(TOK_PERCENT, OP_REM)
        BIND(TOK_AND, OP_BIT_AND)
        BIND(TOK_OR, OP_BIT_OR)
        BIND(TOK_CARET, OP_BIT_XOR)
        BIND(TOK_SHL, OP_SHL)
        BIND(TOK_SHR, OP_SHR)
        BIND(TOK_EQ, OP_ASSIGN)
        BIND(TOK_PLUS_EQ, OP_ADD_ASSIGN)
        BIND(TOK_MINUS_EQ, OP_SUB_ASSIGN)
        BIND(TOK_STAR_EQ, OP_MUL_ASSIGN)
        BIND(TOK_SLASH_EQ, OP_DIV_ASSIGN)
        BIND(TOK_PERCENT_EQ, OP_REM_ASSIGN)
        BIND(TOK_AND_EQ, OP_BIT_AND_ASSIGN)
        BIND(TOK_OR_EQ, OP_BIT_OR_ASSIGN)
        BIND(TOK_CARET_EQ, OP_BIT_XOR_ASSIGN)
        BIND(TOK_SHL_EQ, OP_SHL_ASSIGN)
        BIND(TOK_SHR_EQ, OP_SHR_ASSIGN)
        BIND(TOK_LT, OP_LT)
        BIND(TOK_LE, OP_LE)
        BIND(TOK_GT, OP_GT)
        BIND(TOK_GE, OP_GE)
        BIND(TOK_EE, OP_EQ)
        BIND(TOK_NE, OP_NE)
        BIND(TOK_AND_AND, OP_AND)
        BIND(TOK_OR_OR, OP_OR)
        BIND(TOK_DOT_DOT, OP_RANGE)
        BIND(TOK_DOT_DOT_EQ, OP_RANGE_EQ)
        BIND(TOK_OPEN_PAREN, OP_CALL)
    default:
        break;
    }
    return FU_FALSE;
}

fu_bool_t FuToken_to_suffix_op(FuToken tok, fu_op_k *op) {
    switch (tok.kd) {
        BIND(TOK_QUESTION, OP_CATCH)
    default:
        break;
    }
    return FU_FALSE;
}

#undef BIND

/* token 真正内容从哪里开始 */
fu_size_t FuToken_left_skip_count(FuToken tok) {
    fu_size_t count = 0;
    switch (tok.kd) {
    case TOK_STR:
        /* #..., " */
        count += tok._str.n_hashes + 1;
        break;
    case TOK_RAW_STR:
        /* r, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE_STR:
        /* b, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE_RAW_STR:
        /* b, r,  #..., " */
        count += 1 + 1 + tok._str.n_hashes + 1;
        break;
    case TOK_FORMAT_STR:
        /* f, #..., " */
        count += 1 + tok._str.n_hashes + 1;
        break;
    case TOK_FORMAT_RAW_STR:
        /* f, r,  #..., " */
        count += 1 + 1 + tok._str.n_hashes + 1;
        break;
    case TOK_BYTE:
    case TOK_CHAR:
    case TOK_INT:
    case TOK_FLOAT:
    default:
        FATAL(tok.sp, "can not be here");
    }
    return count;
}

FuStr *FuToken_display(FuToken tok) {
    FuStr *str = FuStr_new();
    char *kind_str = FuKind_token_cstr(tok.kd);
    FuStr_push_utf8_cstr(str, kind_str);
    if (tok.kd < TOK_DOC_COMMENT) {
        return str;
    }
    FuStr_push_utf8_cstr(str, ":");
    FuStr *symbol = FuStr_clone(FuCtx_get_symbol(tok.sp->ctx, tok.sym));
    FuStr_append(str, symbol);
    return str;
}

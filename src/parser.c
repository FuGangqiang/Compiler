#include <assert.h>
#include <stdarg.h>

#include "alloc.h"
#include "log.h"
#include "parse.h"

FuParser *FuParser_new(FuCtx *ctx) {
    FuParser *p = FuMem_new(FuParser);
    p->ctx = ctx;
    p->tok_buf = FuVec_new(sizeof(FuToken));
    p->unclosed_delims = FuVec_new(sizeof(FuToken));
    return p;
}

void FuParser_drop(FuParser *p) {
    if (!p) {
        return;
    }
    FuVec_drop(p->unclosed_delims);
    FuVec_drop(p->tok_buf);
    FuLexer_drop(p->lexer);
    FuMem_free(p);
}

void FuParser_for_file(FuParser *p, char *fpath, fu_size_t len) {
    p->lexer = FuLexer_new(p->ctx);
    FuLexer_for_file(p->lexer, fpath, len);
}

static FuToken FuParser_get_token(FuParser *p) {
    FuToken tok0, tok1, tok2;
    FuSpan *sp;

    tok0 = FuLexer_get_token(p->lexer);
    if (p->in_tok_tree) {
        return tok0;
    }

    switch (tok0.kd) {
    case TOK_NEWLINE:
    case TOK_WHITESPACE:
    case TOK_COMMENT: {
        /* ignore comment, blank */
        do {
            tok0 = FuLexer_get_token(p->lexer);
        } while (FuToken_is_blank(tok0));
        FuLexer_unget_token(p->lexer, tok0);
        return FuParser_get_token(p);
        break;
    }
    case TOK_RAW_IDENT: {
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_NOT) {
            sp = FuSpan_join(tok0.sp, tok1.sp);
            FuStr *name = FuStr_clone(FuCtx_get_symbol(p->ctx, tok0.sym));
            FuStr_push(name, '!');
            fu_sym_t sym = FuCtx_intern_symbol(p->ctx, name);
            return FuToken_new_macro(sp, sym);
        } else {
            FuLexer_unget_token(p->lexer, tok1);
        }
        return FuToken_new_ident(tok0.sp, tok0.sym);
        break;
    }
    case TOK_IDENT: {
        if (tok0.sym < _KW_LAST_UNUSED) {
            return FuToken_new_keyword(tok0.sp, tok0.sym);
        }
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_NOT) {
            sp = FuSpan_join(tok0.sp, tok1.sp);
            FuStr *name = FuStr_clone(FuCtx_get_symbol(p->ctx, tok0.sym));
            FuStr_push(name, '!');
            fu_sym_t sym = FuCtx_intern_symbol(p->ctx, name);
            return FuToken_new_macro(sp, sym);
        } else {
            FuLexer_unget_token(p->lexer, tok1);
        }
        return tok0;
        break;
    }
    case TOK_PLUS: {
        /* `+`, `+=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `+=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_PLUS_EQ, sp);
        } else {
            /* `+` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_MINUS: {
        /* `-`, `-=`, `->` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `-=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_MINUS_EQ, sp);
        } else if (tok1.kd == TOK_GT) {
            /* `->` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_RARROW, sp);
        } else {
            /* `-` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_STAR: {
        /* `*`, `*=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `*=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_STAR_EQ, sp);
        } else {
            /* `*` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_SLASH: {
        /* `/`, `/=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `/=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_SLASH_EQ, sp);
        } else {
            /* `/` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_PERCENT: {
        /* `%`, `%=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `%=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_PERCENT_EQ, sp);
        } else {
            /* `%` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_AND: {
        /* `&`, `&=`, `&&` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `&=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_AND_EQ, sp);
        } else if (tok1.kd == TOK_AND) {
            /* `&&` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_AND_AND, sp);
        } else {
            /* `&` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_OR: {
        /* `|`, `|=`, `||` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `|=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_OR_EQ, sp);
        } else if (tok1.kd == TOK_OR) {
            /* `||` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_OR_OR, sp);
        } else {
            /* `|` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_CARET: {
        /* `^`, `^=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `^=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_CARET_EQ, sp);
        } else {
            /* `^` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_LT: {
        /* `<`, `<<`, `<<=`, `<=`, `<-` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_LT) {
            /* `<<`, `<<=` */
            FuSpan *tmp_sp = FuSpan_unintern_join(tok0.sp, tok1.sp);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_EQ) {
                /* `<<=` */
                sp = FuSpan_join(tmp_sp, tok2.sp);
                FuSpan_drop(tmp_sp);
                return FuToken_new(TOK_SHL_EQ, sp);
            } else {
                /* `<<` */
                FuLexer_unget_token(p->lexer, tok2);
                FuCtx_intern_span(p->ctx, tmp_sp);
                return FuToken_new(TOK_SHL, tmp_sp);
            }
        } else if (tok1.kd == TOK_EQ) {
            /* `<=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_LE, sp);
        } else if (tok1.kd == TOK_MINUS) {
            /* `<-` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_LARROW, sp);
        } else {
            /* `<` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_GT: {
        /* `>`, `>>` `>>=`, `>=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_GT) {
            /* `>>`, `>>=` */
            FuSpan *tmp_sp = FuSpan_unintern_join(tok0.sp, tok1.sp);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_EQ) {
                /* `>>=` */
                sp = FuSpan_join(tmp_sp, tok2.sp);
                FuSpan_drop(tmp_sp);
                return FuToken_new(TOK_SHR_EQ, sp);
            } else {
                /* `>>` */
                FuLexer_unget_token(p->lexer, tok2);
                FuCtx_intern_span(p->ctx, tmp_sp);
                return FuToken_new(TOK_SHR, tmp_sp);
            }
        } else if (tok1.kd == TOK_EQ) {
            /* `>=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_GE, sp);
        } else {
            /* `>` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_EQ: {
        /* `=`, `==`, `=>` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `==` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_EE, sp);
        } else if (tok1.kd == TOK_GT) {
            /* `=>` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_FAT_ARROW, sp);
        } else {
            /* `=` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_NOT: {
        /* `!`, `!=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `!=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_NE, sp);
        } else {
            /* `!` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_DOT: {
        /* `.`, `..`, `...`, `..=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_DOT) {
            /* `..`, `...`, `..=` */
            FuSpan *tmp_sp = FuSpan_unintern_join(tok0.sp, tok1.sp);
            tok2 = FuLexer_get_token(p->lexer);
            if (tok2.kd == TOK_DOT) {
                /* `...` */
                sp = FuSpan_join(tmp_sp, tok2.sp);
                FuSpan_drop(tmp_sp);
                return FuToken_new(TOK_DOT_DOT_DOT, sp);
            } else if (tok2.kd == TOK_EQ) {
                /* `..=` */
                sp = FuSpan_join(tmp_sp, tok2.sp);
                FuSpan_drop(tmp_sp);
                return FuToken_new(TOK_DOT_DOT_EQ, sp);
            } else {
                /* `..` */
                FuLexer_unget_token(p->lexer, tok2);
                FuCtx_intern_span(p->ctx, tmp_sp);
                return FuToken_new(TOK_DOT_DOT, tmp_sp);
            }
        } else {
            /* `.` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    case TOK_COLON: {
        /* `:`, `::` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_COLON) {
            /* `::` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_MOD_SEP, sp);
        } else {
            /* `:` */
            FuLexer_unget_token(p->lexer, tok1);
            return tok0;
        }
        break;
    }
    default:
        return tok0;
        break;
    }
}

static FuToken FuParser_nth_token(FuParser *p, fu_size_t n) {
    while (n >= FuVec_len(p->tok_buf)) {
        FuToken tok = FuParser_get_token(p);
        FuVec_push(p->tok_buf, &tok);
    }
    return *(FuToken *)FuVec_get(p->tok_buf, n);
}

static fu_bool_t FuParser_is_eof(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_EOF) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static FuToken FuParser_bump(FuParser *p) {
    FuToken cur_tok = FuParser_nth_token(p, 0);
    FuVec_remove_slice(p->tok_buf, 0, 1, NULL);
    return cur_tok;
}

static FuSpan *FuParser_current_span(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    return tok.sp;
}

static FuToken FuParser_expect_keyword(FuParser *p, fu_keyword_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        FATAL2(tok.sp, "expect `%s`, find `%s`", FuKind_keyword_cstr(kd), FuToken_kind_csr(tok));
    }
    if (tok.sym != kd) {
        FATAL2(tok.sp, "expect `%s`, find `%s`", FuKind_keyword_cstr(kd), FuToken_kind_csr(tok));
    }
    return FuParser_bump(p);
}

static FuToken FuParser_expect_token(FuParser *p, fu_token_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != kd) {
        FATAL2(tok.sp, "expect `%s`, find `%s`", FuKind_token_cstr(kd), FuToken_kind_csr(tok));
    }
    return FuParser_bump(p);
}

static FuToken FuParser_expect_token_fn(FuParser *p, FuCheckTokenFn fn, char *wanted) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!fn(tok)) {
        FATAL2(tok.sp, "expect `%s`, find `%s`", wanted, FuToken_kind_csr(tok));
    }
    return FuParser_bump(p);
}

static fu_bool_t FuParser_check_keyword(FuParser *p, fu_keyword_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return FU_FALSE;
    }
    if (tok.sym != kd) {
        return FU_FALSE;
    }
    return FU_TRUE;
}

static fu_bool_t FuParser_check_token(FuParser *p, fu_token_k kd) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == kd) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_token_fn(FuParser *p, FuCheckTokenFn fn) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (fn(tok)) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_eat_keyword(FuParser *p, fu_keyword_k kw) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return FU_FALSE;
    }
    if (tok.sym == kw) {
        FuParser_bump(p);
        return FU_TRUE;
    }
    return FU_FALSE;
}

FuLit *FuParser_parse_lit(FuParser *p) {
    FuToken tok = FuParser_expect_token_fn(p, FuToken_is_lit, "literal");
    FuLit *lit = NULL;
    switch (tok.kd) {
    case TOK_KEYWORD:
        if (tok.sym == KW_NIL) {
            lit = FuToken_to_lit_nil(tok);
        } else if (tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
            lit = FuToken_to_lit_bool(tok);
        } else {
            FATAL1(tok.sp, "expect literal, find `%s`", FuToken_kind_csr(tok));
        }
        break;
    case TOK_BYTE:
        lit = FuToken_to_lit_byte(tok);
        break;
    case TOK_CHAR:
        lit = FuToken_to_lit_char(tok);
        break;
    case TOK_INT:
        lit = FuToken_to_lit_int(tok);
        break;
    case TOK_FLOAT:
        lit = FuToken_to_lit_float(tok);
        break;
    case TOK_STR:
    case TOK_RAW_STR:
        lit = FuToken_to_lit_str(tok);
        break;
    case TOK_BYTE_STR:
    case TOK_BYTE_RAW_STR:
        lit = FuToken_to_lit_byte_str(tok);
        break;
    case TOK_FORMAT_STR:
    case TOK_FORMAT_RAW_STR:
        FATAL1(tok.sp, "unimplemented lit: `%s`", FuToken_kind_csr(tok));
        break;
    default:
        FATAL(tok.sp, "can not be here");
    }
    return lit;
}

FuIdent *FuParser_parse_ident(FuParser *p) {
    FuSpan *sp = FuParser_current_span(p);
    FuToken tok = FuParser_expect_token_fn(p, FuToken_is_ident, "expect ident");
    FuIdent *ident = FuMem_new(FuIdent);
    ident->sp = sp;
    ident->name = tok.sym;
    return ident;
}

static FuPathItem *FuParser_parse_path_item(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuIdent *ident = FuParser_parse_ident(p);
    FuVec *ge_args = NULL;
    /* todo: generic
    if (FuParser_check_2_token(p, TOK_POUND, TOK_LT)) {
        ge_args = FuParser_parse_ge_args(p);
    }
    */
    FuPathItem *item = FuMem_new(FuPathItem);
    item->sp = FuSpan_join(lo, FuParser_current_span(p));
    item->ident = ident;
    item->ge_args = ge_args;
    return item;
}

/* todo: generic */
FuPath *FuParser_parse_path(FuParser *p) {
    FuPath *path = FuMem_new(FuPath);
    path->segments = FuVec_new(sizeof(FuPathItem *));
    while (1) {
        FuPathItem *item = FuParser_parse_path_item(p);
        FuVec_push_ptr(path->segments, item);
        FuToken tok0 = FuParser_nth_token(p, 0);
        FuToken tok1 = FuParser_nth_token(p, 1);
        if (tok0.kd == TOK_MOD_SEP && tok1.kd == TOK_IDENT) {
            FuParser_bump(p);
        } else {
            break;
        }
    }
    FuPathItem *start = FuVec_first_ptr(path->segments);
    FuPathItem *end = FuVec_last_ptr(path->segments);
    path->sp = FuSpan_join(start->sp, end->sp);
    return path;
}

FuLabel *FuParser_parse_label(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_LABEL) {
        return NULL;
    }
    FuParser_bump(p);
    return FuLabel_new(tok.sp, tok.sym);
}

static FuType *FuParser_parse_path_type(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!FuParser_check_token_fn(p, FuToken_is_ident)) {
        FATAL1(tok.sp, "expect ident, find: `%s`", FuToken_kind_csr(tok));
    }
    FuPath *path = FuParser_parse_path(p);
    return FuType_new_path(p->ctx, path);
}

static FuType *FuParser_parse_keyword_type(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        FATAL1(tok.sp, "expect keyword type, find: `%s`", FuToken_kind_csr(tok));
    }
    switch (tok.sym) {
    case KW_NIL:
    case KW_UNDERSCORE:
        FuParser_bump(p);
        return FuType_from_keyword(p->ctx, tok.sp, tok.sym);
        break;
    case KW_SELF_UPPER:
    case KW_SELF_LOWER:
        return FuParser_parse_path_type(p);
        break;
    default:
        FATAL1(tok.sp, "invalid type: `%s`", FuToken_kind_csr(tok));
        break;
    }
    return NULL;
}

static FuType *FuParser_parse_pointer_type(FuParser *p, fu_op_prec_t prec) {
    static fu_sym_t raw_sym = 0;
    if (!raw_sym) {
        FuStr *raw = FuStr_from_utf8_cstr("raw");
        raw_sym = FuCtx_intern_symbol(p->ctx, raw);
    }

    FuToken tok0, tok1;
    fu_bool_t is_raw = FU_FALSE;

    tok0 = FuParser_expect_token(p, TOK_STAR);
    tok1 = FuParser_nth_token(p, 0);
    if (FuToken_is_ident(tok1) && tok1.sym == raw_sym) {
        FuParser_bump(p);
        is_raw = FU_TRUE;
    }
    FuType *ty;
    FuType *right = FuParser_parse_type(p, prec, FU_FALSE);
    FuSpan *sp = FuSpan_join(tok0.sp, right->sp);
    if (is_raw) {
        ty = FuType_new(p->ctx, sp, TY_RAW_PTR);
        ty->_raw_ptr = right;
    } else {
        ty = FuType_new(p->ctx, sp, TY_PTR);
        ty->_ptr = right;
    }
    return ty;
}

static FuType *FuParser_parse_array_slice_type(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuType *inner = FuParser_parse_type(p, 0, FU_TRUE);
    FuToken close_tok;
    FuType *ty;
    FuSpan *sp;
    if (FuParser_check_token(p, TOK_SEMI)) {
        FuParser_bump(p);
        FuExpr *size = FuParser_parse_expr(p, 0, FU_TRUE);
        close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
        sp = FuSpan_join(open_tok.sp, close_tok.sp);
        ty = FuType_new(p->ctx, sp, TY_ARRAY);
        ty->_array.ty = inner;
        ty->_array.size = size;
        return ty;
    }
    close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    sp = FuSpan_join(open_tok.sp, close_tok.sp);
    ty = FuType_new(p->ctx, sp, TY_SLICE);
    ty->_slice = inner;
    return ty;
}

static FuType *FuParser_parse_group_or_tuple_type(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    if (FuParser_check_token(p, TOK_CLOSE_PAREN)) {
        FuParser_bump(p);
        return ty;
    }
    FuVec *tuple = FuVec_new(sizeof(FuType *));
    FuType *item = ty;
    while (1) {
        FuVec_push_ptr(tuple, item);
        if (FuParser_check_token(p, TOK_COMMA)) {
            FuParser_bump(p);
        } else {
            break;
        }
        item = FuParser_parse_type(p, 0, FU_TRUE);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    ty = FuType_new(p->ctx, sp, TY_TUPLE);
    ty->_tuple = tuple;
    return ty;
}

static FuType *FuParser_parse_prefix_type(FuParser *p, fu_ty_op_k op, fu_op_prec_t prec) {
    FuToken tok = FuParser_nth_token(p, 0);
    switch (op) {
    case TY_OP_PTR:
        return FuParser_parse_pointer_type(p, prec);
        break;
    default:
        break;
    }
    FATAL1(tok.sp, "invalid prefix type operatro: `%s`", FuKind_ty_op_cstr(op));
    return NULL;
}

static FuType *FuParser_parse_suffix_type(FuParser *p, FuType *left, fu_ty_op_k op) {
    FuToken op_tok = FuParser_nth_token(p, 0);
    FuSpan *sp = FuSpan_join(op_tok.sp, left->sp);
    switch (op) {
    case TY_OP_NILABLE: {
        FuParser_bump(p);
        FuType *ty = FuType_new(p->ctx, sp, TY_NILABLE);
        ty->_nilable = left;
        return ty;
        break;
    }
    default: {
        break;
    }
    }
    FATAL1(op_tok.sp, "invalid suffix type operator: `%s`", FuToken_kind_csr(op_tok));
    /* can not be here */
    return NULL;
}

static FuType *FuParser_parse_infix_type(FuParser *p, FuType *left, fu_ty_op_k op, fu_op_prec_t prec) {
    FuToken op_tok = FuParser_nth_token(p, 0);
    FuSpan *old_left_sp = left->sp;
    switch (op) {
    case TY_OP_TRANS: {
        FuVec *sig_tys = FuVec_new(sizeof(FuType *));
        while (1) {
            FuVec_push_ptr(sig_tys, left);
            if (!FuParser_check_token(p, TOK_RARROW)) {
                break;
            }
            FuParser_bump(p);
            left = FuParser_parse_type(p, prec, FU_FALSE);
            if (!left) {
                break;
            }
        }
        FuFnSig *sig = FuFnSig_new(NULL, sig_tys);
        FuSpan *sp = FuSpan_join(left->sp, old_left_sp);
        FuType *fn = FuType_new(p->ctx, sp, TY_FN_SIG);
        fn->_fn_sig = sig;
        return fn;
        break;
    }
    default:
        break;
    }
    FATAL1(op_tok.sp, "invalid infix type operator: `%s`", FuToken_kind_csr(op_tok));
    return NULL;
}

FuType *FuParser_parse_type(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null) {
    FuToken tok = FuParser_nth_token(p, 0);
    FuType *prefix_type;
    switch (tok.kd) {
    case TOK_OPEN_PAREN:
        prefix_type = FuParser_parse_group_or_tuple_type(p);
        break;
    case TOK_KEYWORD:
        prefix_type = FuParser_parse_keyword_type(p);
        break;
    case TOK_IDENT:
        prefix_type = FuParser_parse_path_type(p);
        break;
    case TOK_OPEN_BRACKET:
        prefix_type = FuParser_parse_array_slice_type(p);
        break;
    default: {
        fu_ty_op_k prefix_op;
        if (!FuToken_to_prefix_ty_op(tok, &prefix_op)) {
            if (check_null) {
                FATAL1(tok.sp, "expect type, find `%s`", FuToken_kind_csr(tok));
            } else {
                return NULL;
            }
        }
        prefix_type = FuParser_parse_prefix_type(p, prefix_op, FuTyOp_precedence(prefix_op));
        break;
    }
    }
    FuType *left = prefix_type;
    while (1) {
        tok = FuParser_nth_token(p, 0);

        fu_ty_op_k suffix_op;
        if (FuToken_to_suffix_ty_op(tok, &suffix_op) && prec < FuTyOp_precedence(suffix_op)) {
            left = FuParser_parse_suffix_type(p, left, suffix_op);
            continue;
        }

        fu_ty_op_k infix_op;
        if (FuToken_to_infix_ty_op(tok, &infix_op)) {
            fu_op_prec_t infix_prec = FuTyOp_precedence(infix_op);
            if (prec < infix_prec) {
                left = FuParser_parse_infix_type(p, left, infix_op, infix_prec);
                continue;
            }
        }
        break;
    }
    return left;
}

static FuPat *FuParser_parse_bind_pat(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    fu_bool_t is_ref = FU_FALSE;
    if (tok.kd == TOK_STAR) {
        is_ref = FU_TRUE;
        FuParser_bump(p);
    }
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_AT);
    FuPat *bind = FuParser_parse_pat(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(ident->sp, bind->sp);
    FuPat *pat = FuPat_new(sp, PAT_BIND);
    pat->_bind.is_ref = is_ref;
    pat->_bind.ident = ident;
    pat->_bind.pat = bind;
    return pat;
}

static FuPat *FuParser_parse_struct_pat(FuParser *p, FuExpr *path) {
    FuParser_expect_token(p, TOK_MOD_SEP);
    FuToken tok = FuParser_nth_token(p, 0);
    fu_pat_k kd;
    if (tok.kd == TOK_OPEN_PAREN) {
        FuParser_expect_token(p, TOK_OPEN_PAREN);
        kd = PAT_TUPLE_STRUCT;
    } else if (tok.kd == TOK_OPEN_BRACE) {
        FuParser_expect_token(p, TOK_OPEN_BRACE);
        kd = PAT_STRUCT;
    } else {
        FATAL1(tok.sp, "expect `(` or `{` to build struct pattern, find `%s`", FuToken_kind_csr(tok));
    }
    FuVec *pats = FuVec_new(sizeof(FuPat *));
    while (1) {
        FuPat *item = FuParser_parse_pat(p, 0, FU_TRUE);
        FuVec_push_ptr(pats, item);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_COMMA) {
            FuParser_expect_token(p, TOK_COMMA);
            continue;
        }
        if ((kd == PAT_STRUCT && tok.kd == TOK_CLOSE_BRACE) || (kd == PAT_TUPLE_STRUCT && tok.kd == TOK_CLOSE_PAREN)) {
            break;
        }
    }
    FuToken close_tok;
    if (kd == PAT_STRUCT) {
        close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    } else {
        close_tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    }
    FuPat *pat;
    FuSpan *sp = FuSpan_join(path->sp, close_tok.sp);
    if (kd == PAT_STRUCT) {
        pat = FuPat_new(sp, PAT_STRUCT);
        pat->_struct.path = path;
        pat->_struct.pats = pats;
        return pat;
    }
    pat = FuPat_new(sp, PAT_TUPLE_STRUCT);
    pat->_tuple_struct.path = path;
    pat->_tuple_struct.pats = pats;
    return pat;
}

static FuPat *FuParser_parse_dot_pat(FuParser *p) {
    FuToken start_tok = FuParser_expect_token(p, TOK_DOT);
    FuToken tok = FuParser_nth_token(p, 0);
    FuSpan *sp;
    if (tok.kd == TOK_IDENT) {
        FuIdent *ident = FuParser_parse_ident(p);
        sp = FuSpan_join(start_tok.sp, ident->sp);
        FuPat *pat = FuPat_new(sp, PAT_FIELD);
        pat->_field = ident;
        return pat;
    }
    FuLit *lit = FuParser_parse_lit(p);
    if (lit->kd != LIT_INT) {
        FATAL1(tok.sp, "expect int or identifer, find: `%s`", FuToken_kind_csr(tok));
    }
    sp = FuSpan_join(start_tok.sp, lit->sp);
    FuPat *pat = FuPat_new(sp, PAT_INDEX);
    pat->_index = lit;
    return pat;
}

static FuPat *FuParser_parse_repeat_pat(FuParser *p, FuPat *left) {
    FuToken tok = FuParser_expect_token(p, TOK_DOT_DOT_DOT);
    FuSpan *sp;
    if (left) {
        sp = FuSpan_join(tok.sp, left->sp);
    } else {
        sp = tok.sp;
    }
    FuPat *pat = FuPat_new(sp, PAT_REPEAT);
    pat->_repeat = left;
    return pat;
}

static FuPat *FuParser_parse_base_pat(FuParser *p) {
    FuToken tok = FuParser_expect_token(p, TOK_DOT_DOT_DOT);
    FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE);
    FuSpan *sp = FuSpan_join(tok.sp, expr->sp);
    FuPat *pat = FuPat_new(sp, PAT_BASE);
    pat->_base = expr;
    return pat;
}

static FuPat *FuParser_parse_slice_pat(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuVec *pats = FuVec_new(sizeof(FuPat *));
    while (1) {
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_CLOSE_BRACKET) {
            break;
        }
        FuPat *item = FuParser_parse_pat(p, 0, FU_TRUE);
        FuVec_push_ptr(pats, item);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuPat *pat = FuPat_new(sp, PAT_SLICE);
    pat->_slice.pats = pats;
    return pat;
}

static FuPat *FuParser_parse_group_tuple_pat(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuPat *item = FuParser_parse_pat(p, 0, FU_TRUE);
    if (FuParser_check_token(p, TOK_CLOSE_PAREN)) {
        FuParser_expect_token(p, TOK_CLOSE_PAREN);
        return item;
    }
    FuVec *pats = FuVec_new(sizeof(FuPat *));
    while (1) {
        FuVec_push_ptr(pats, item);
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
        item = FuParser_parse_pat(p, 0, FU_TRUE);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuPat *pat = FuPat_new(sp, PAT_TUPLE);
    pat->_tuple.pats = pats;
    return pat;
}

static FuPat *FuParser_parse_or_pat(FuParser *p, FuPat *left) {
    FuSpan *old_left_sp = left->sp;
    FuVec *pats = FuVec_new(sizeof(FuPat *));
    while (1) {
        FuVec_push_ptr(pats, left);
        if (!FuParser_check_token(p, TOK_OR)) {
            break;
        }
        FuParser_bump(p);
        left = FuParser_parse_pat(p, 1, FU_TRUE);
    }
    FuSpan *sp = FuSpan_join(left->sp, old_left_sp);
    FuPat *pat = FuPat_new(sp, PAT_OR);
    pat->_or.pats = pats;
    return pat;
}

FuPat *FuParser_parse_pat(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null) {
    FuPat *prefix_pat = NULL;
    FuToken tok0 = FuParser_nth_token(p, 0);
    FuToken tok1;
    switch (tok0.kd) {
    case TOK_BYTE:
    case TOK_CHAR:
    case TOK_INT:
    case TOK_FLOAT:
    case TOK_STR:
    case TOK_RAW_STR:
    case TOK_BYTE_STR:
    case TOK_BYTE_RAW_STR:
    case TOK_FORMAT_STR:
    case TOK_FORMAT_RAW_STR: {
        FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE);
        FuPat *pat = FuPat_new(expr->sp, PAT_EXPR);
        pat->_expr = expr;
        tok0 = FuParser_nth_token(p, 0);
        if (tok0.kd == TOK_DOT_DOT_DOT) {
            prefix_pat = FuParser_parse_repeat_pat(p, pat);
            break;
        }
        prefix_pat = pat;
        break;
    }
    case TOK_KEYWORD: {
        if (tok0.sym == KW_UNDERSCORE) {
            tok0 = FuParser_expect_keyword(p, KW_UNDERSCORE);
            FuPat *left = FuPat_new(tok0.sp, PAT_WILD);
            tok1 = FuParser_nth_token(p, 0);
            if (tok1.kd == TOK_DOT_DOT_DOT) {
                prefix_pat = FuParser_parse_repeat_pat(p, left);
                break;
            }
            prefix_pat = left;
            break;
        }
        FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE);
        prefix_pat = FuPat_new(expr->sp, PAT_EXPR);
        prefix_pat->_expr = expr;
        break;
    }
    case TOK_IDENT: {
        tok1 = FuParser_nth_token(p, 1);
        if (tok1.kd == TOK_AT) {
            prefix_pat = FuParser_parse_bind_pat(p);
            break;
        }
        FuPath *path = FuParser_parse_path(p);
        FuExpr *expr = FuExpr_new(path->sp, EXPR_PATH);
        expr->_path.path = path;
        tok0 = FuParser_nth_token(p, 0);
        if (tok0.kd == TOK_MOD_SEP) {
            prefix_pat = FuParser_parse_struct_pat(p, expr);
            break;
        }
        /* only allow path */
        prefix_pat = FuPat_new(path->sp, PAT_EXPR);
        prefix_pat->_expr = expr;
        break;
    }
    case TOK_STAR:
        prefix_pat = FuParser_parse_bind_pat(p);
        break;
    case TOK_DOT:
        prefix_pat = FuParser_parse_dot_pat(p);
        break;
    case TOK_DOT_DOT_DOT: {
        tok1 = FuParser_nth_token(p, 1);
        if (tok1.kd != TOK_IDENT) {
            prefix_pat = FuParser_parse_repeat_pat(p, NULL);
            break;
        }
        prefix_pat = FuParser_parse_base_pat(p);
        break;
    }
    case TOK_OPEN_BRACKET:
        prefix_pat = FuParser_parse_slice_pat(p);
        break;
    case TOK_OPEN_PAREN:
        prefix_pat = FuParser_parse_group_tuple_pat(p);
        break;
    default: {
        if (check_null) {
            FATAL1(tok0.sp, "invalid pat: `%s`", FuToken_kind_csr(tok0));
        } else {
            return NULL;
        }
        break;
    }
    }
    tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd != TOK_OR) {
        return prefix_pat;
    }
    if (prec == 0) {
        return FuParser_parse_or_pat(p, prefix_pat);
    }
    return prefix_pat;
}

static FuVec *FuParser_parse_fn_args(FuParser *p) {
    FuVec *args = FuVec_new(sizeof(FuExpr *));
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (!expr) {
        return args;
    }
    FuToken tok;
    while (1) {
        FuVec_push_ptr(args, expr);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_bump(p);
        expr = FuParser_parse_expr(p, 0, FU_TRUE);
    }
    return args;
}

static FuExpr *FuParser_parse_call_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuVec *args = FuParser_parse_fn_args(p);
    FuToken tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_CALL);
    expr->_call.base = left;
    expr->_call.args = args;
    return expr;
}

static FuExpr *FuParser_parse_method_call_expr(FuParser *p, FuExpr *left) {
    FuPathItem *method = FuParser_parse_path_item(p);
    FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuVec *args = FuParser_parse_fn_args(p);
    FuToken tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuVec_insert_ptr(args, 0, left);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_METHOD_CALL);
    expr->_method_call.method = method;
    expr->_method_call.args = args;
    return expr;
}

static FuExpr *FuParser_parse_field_expr(FuParser *p, FuExpr *left) {
    FuIdent *ident;
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == TOK_INT) {
        /* `tup.0` */
        ident = FuToken_index_to_ident(tok0);
        FuParser_bump(p);
    } else {
        /* `base.field` */
        ident = FuParser_parse_ident(p);
    }
    FuSpan *sp = FuSpan_join(ident->sp, left->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_FIELD);
    expr->_field.base = left;
    expr->_field.ident = ident;
    return expr;
}

static FuExpr *FuParser_parse_dot_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_DOT);
    FuToken tok0 = FuParser_nth_token(p, 0);
    FuToken tok1 = FuParser_nth_token(p, 1);
    if (tok0.kd == TOK_IDENT && (tok1.kd == TOK_OPEN_PAREN || tok1.kd == TOK_POUND)) {
        /* `base.method_call()`, `base.method_call#<>()` */
        return FuParser_parse_method_call_expr(p, left);
    }
    if (tok0.kd == TOK_MACRO) {
        /* todo: macro */
        FATAL(tok0.sp, "unimplemented: suffix macro");
    }
    return FuParser_parse_field_expr(p, left);
}

static FuExpr *FuParser_parse_index_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuToken tok;
    FuExpr *idx = FuParser_parse_expr(p, 0, FU_TRUE);
    tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_INDEX);
    expr->_index.base = left;
    expr->_index.idx = idx;
    return expr;
}

static FuFieldInit *FuParser_parse_field_init(FuParser *p) {
    FuToken tok0, tok1;
    FuSpan *sp;
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: parse attr */
    tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == TOK_DOT) {
        tok1 = FuParser_nth_token(p, 1);
        FuParser_bump(p);
        if (tok1.kd == TOK_IDENT) {
            FuIdent *ident = FuParser_parse_ident(p);
            FuParser_expect_token(p, TOK_EQ);
            FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
            sp = FuSpan_join(tok0.sp, expr->sp);
            FuFieldInit *init = FuFieldInit_new(sp, FLD_NAME, attrs);
            init->_name.ident = ident;
            init->_name.init = expr;
            return init;
        } else if (tok1.kd == TOK_INT) {
            FuLit *lit = FuParser_parse_lit(p);
            FuParser_expect_token(p, TOK_EQ);
            FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
            sp = FuSpan_join(tok0.sp, expr->sp);
            FuFieldInit *init = FuFieldInit_new(sp, FLD_INDEX, attrs);
            init->_index.lit = lit;
            init->_index.init = expr;
            return init;
        }
        FATAL1(tok1.sp, "expect identifier name or array index, find `%s`", FuToken_kind_csr(tok1));
    }
    if (tok0.kd == TOK_DOT_DOT_DOT) {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
        sp = FuSpan_join(tok0.sp, expr->sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_BASE, attrs);
        init->_base = expr;
        return init;
    }
    if (tok0.kd == TOK_SEMI) {
        FuParser_bump(p);
        FuExpr *size = FuParser_parse_expr(p, 0, FU_TRUE);
        sp = FuSpan_join(tok0.sp, size->sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_SIZE, attrs);
        init->_size = size;
        return init;
    }
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    tok1 = FuParser_nth_token(p, 0);
    if (tok1.kd == TOK_DOT_DOT_DOT) {
        FuParser_bump(p);
        sp = FuSpan_join(expr->sp, tok1.sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_REPEAT, attrs);
        init->_repeat = expr;
        return init;
    }
    FuFieldInit *init = FuFieldInit_new(expr->sp, FLD_EXPR, attrs);
    init->_expr = expr;
    return init;
}

static FuVec *FuParser_parse_field_inits(FuParser *p) {
    FuToken tok;
    FuVec *inits = FuVec_new(sizeof(FuNode *));
    while (1) {
        FuFieldInit *init = FuParser_parse_field_init(p);
        FuVec_push_ptr(inits, init);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_bump(p);
    }
    return inits;
}

static FuExpr *FuParser_parse_struct_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_MOD_SEP);
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_OPEN_BRACE) {
        FuParser_bump(p);
        FuVec *inits = FuParser_parse_field_inits(p);
        FuToken tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
        FuSpan *sp = FuSpan_join(left->sp, tok.sp);
        FuExpr *expr = FuExpr_new(sp, EXPR_STRUCT);
        expr->_struct.base = left;
        expr->_struct.field_inits = inits;
        return expr;
    }
    if (tok.kd == TOK_OPEN_PAREN) {
        FuParser_bump(p);
        FuVec *inits = FuParser_parse_field_inits(p);
        FuToken tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
        FuSpan *sp = FuSpan_join(left->sp, tok.sp);
        FuExpr *expr = FuExpr_new(sp, EXPR_TUPLE_STRUCT);
        expr->_tuple_struct.base = left;
        expr->_tuple_struct.field_inits = inits;
        return expr;
    }
    FATAL1(tok.sp, "expect `(` or `{` to build struct, find: `%s`", FuToken_kind_csr(tok));
    return NULL;
}

static FuExpr *FuParser_parse_array_expr(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuVec *inits = FuParser_parse_field_inits(p);
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuExpr *array_expr = FuExpr_new(sp, EXPR_ARRAY);
    array_expr->_array.field_inits = inits;
    return array_expr;
}

static FuExpr *FuParser_parse_range_expr(FuParser *p, FuExpr *left, fu_op_k op, fu_op_prec_t prec) {
    FuSpan *start_sp;
    FuSpan *end_sp;
    if (left) {
        start_sp = left->sp;
    } else {
        start_sp = FuParser_current_span(p);
    }
    FuToken op_tok = FuParser_bump(p);
    FuExpr *right = FuParser_parse_expr(p, prec, FU_FALSE);
    if (right) {
        end_sp = right->sp;
    } else {
        end_sp = op_tok.sp;
    }
    FuSpan *sp = FuSpan_join(start_sp, end_sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_RANGE);
    expr->_range.is_inclusive = op == OP_RANGE_EQ ? FU_TRUE : FU_FALSE;
    expr->_range.start = left;
    expr->_range.end = right;
    return expr;
}

static FuExpr *FuParser_parse_cast_expr(FuParser *p, FuExpr *left, fu_op_k op, fu_op_prec_t prec) {
    FuParser_expect_keyword(p, KW_AS);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(left->sp, ty->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_CAST);
    expr->_cast.expr = left;
    expr->_cast.ty = ty;
    return expr;
}

static FuExpr *FuParser_parse_prefix_expr(FuParser *p, fu_op_k op, fu_op_prec_t prec) {
    FuExpr *expr;
    switch (op) {
    case OP_RANGE:
    case OP_RANGE_EQ:
        expr = FuParser_parse_range_expr(p, NULL, op, prec);
        break;
    default: {
        FuToken op_tok = FuParser_bump(p);
        FuExpr *right = FuParser_parse_expr(p, prec, FU_TRUE);
        FuSpan *sp = FuSpan_join(op_tok.sp, right->sp);
        expr = FuExpr_new(sp, EXPR_UNARY);
        expr->_unary.op = op;
        expr->_unary.op_sp = sp;
        expr->_unary.expr = right;
        break;
    }
    }
    return expr;
}

static FuExpr *FuParser_parse_infix_expr(FuParser *p, FuExpr *left, fu_op_k op, fu_op_prec_t prec) {
    FuToken op_tok = FuParser_nth_token(p, 0);
    switch (op) {
        /* todo: macro */
    case OP_DOT:
        return FuParser_parse_dot_expr(p, left);
        break;
    case OP_CALL:
        return FuParser_parse_call_expr(p, left);
        break;
    case OP_INDEX:
        return FuParser_parse_index_expr(p, left);
        break;
    case OP_STRUCT:
        return FuParser_parse_struct_expr(p, left);
        break;
    case OP_RANGE:
    case OP_RANGE_EQ: {
        return FuParser_parse_range_expr(p, left, op, prec);
        break;
    }
    case OP_CAST:
        return FuParser_parse_cast_expr(p, left, op, prec);
        break;
    default: {
        FuParser_bump(p);
        FuExpr *right = FuParser_parse_expr(p, prec, FU_TRUE);
        FuSpan *sp = FuSpan_join(left->sp, right->sp);
        FuExpr *expr = FuExpr_new(sp, EXPR_BINARY);
        expr->_binary.op = op;
        expr->_binary.op_sp = op_tok.sp;
        expr->_binary.lexpr = left;
        expr->_binary.rexpr = right;
        return expr;
        break;
    }
    }
    /* can not be here */
    return NULL;
}

static FuExpr *FuParser_parse_suffix_expr(FuParser *p, FuExpr *left, fu_op_k op) {
    FuToken op_tok = FuParser_nth_token(p, 0);
    FuSpan *sp = FuSpan_join(op_tok.sp, left->sp);
    switch (op) {
    case OP_CATCH: {
        FuParser_bump(p);
        FuExpr *expr = FuExpr_new(sp, EXPR_CATCH);
        expr->_catch.op = op;
        expr->_catch.op_sp = op_tok.sp;
        expr->_catch.expr = left;
        break;
    }
    default: {
        FuParser_bump(p);
        FuExpr *expr = FuExpr_new(sp, EXPR_UNARY);
        expr->_unary.op = op;
        expr->_unary.op_sp = op_tok.sp;
        expr->_unary.expr = left;
        return expr;
    }
    }
    /* can not be here */
    return NULL;
}

static FuExpr *FuParser_parse_let_cond_expr(FuParser *p) {
    FuToken tok = FuParser_expect_keyword(p, KW_LET);
    FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(tok.sp, expr->sp);
    FuExpr *cond = FuExpr_new(sp, EXPR_LET_COND);
    cond->_let_cond.pat = pat;
    cond->_let_cond.expr = expr;
    return cond;
}

static FuExpr *FuParser_parse_group_or_tuple_expr(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuToken tok = FuParser_bump(p);

    /* group expression */
    if (tok.kd == TOK_CLOSE_PAREN) {
        return expr;
    }
    /* tuple expression */
    if (tok.kd != TOK_COMMA) {
        FATAL1(tok.sp, "expect `)`, find tok: `%s`", FuToken_kind_csr(tok));
    }

    FuVec *fields = FuVec_new(sizeof(FuExpr *));
    FuVec_push_ptr(fields, expr);
    while (1) {
        expr = FuParser_parse_expr(p, 0, FU_TRUE);
        FuVec_push_ptr(fields, expr);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
    }
    tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(open_tok.sp, tok.sp);
    FuExpr *tuple_expr = FuExpr_new(sp, EXPR_TUPLE);
    tuple_expr->_tuple.fields = fields;
    return tuple_expr;
}

static fu_bool_t FuParser_check_block(FuParser *p) {
    FuToken tok, tok1;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd == TOK_EOF) {
            return FU_FALSE;
        }
        if (tok.kd == TOK_OPEN_BRACE) {
            return FU_TRUE;
        }
        if (tok.kd == TOK_KEYWORD && (tok.sym == KW_ASYNC || tok.sym == KW_UNSAFE)) {
            i++;
            continue;
        }
        if (tok.kd == TOK_LABEL) {
            tok1 = FuParser_nth_token(p, i + 1);
            if (tok1.kd != TOK_COLON) {
                FATAL1(tok1.sp, "expect `:`, find `%s`", FuToken_kind_csr(tok1));
            }
            i += 2;
            continue;
        }
        break;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_fn(FuParser *p) {
    FuToken tok;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd == TOK_EOF) {
            return FU_FALSE;
        }
        if (tok.kd != TOK_KEYWORD) {
            return FU_FALSE;
        }
        if (tok.sym == KW_FN) {
            return FU_TRUE;
        }
        if (tok.sym == KW_ASYNC || tok.sym == KW_UNSAFE || tok.sym == KW_CONST) {
            i++;
            continue;
        }
        break;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_loop(FuParser *p, fu_keyword_k keyword) {
    FuToken tok, tok1;
    tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_LABEL) {
        tok1 = FuParser_nth_token(p, 1);
        if (tok1.kd != TOK_COLON) {
            FATAL1(tok1.sp, "expect `:`, find `%s`", FuToken_kind_csr(tok1));
        }
        tok = FuParser_nth_token(p, 2);
    }
    if (tok.kd == TOK_EOF) {
        return FU_FALSE;
    }
    if (tok.kd == TOK_KEYWORD && tok.sym == keyword) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

FuNode *FuParser_parse_block_item(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: attrs */
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_KEYWORD) {
        switch (tok.sym) {
        case KW_USE:
            return FuParser_parse_item_use(p, attrs, VIS_PRIV);
            break;
        case KW_STATIC:
            return FuParser_parse_item_static(p, attrs, VIS_PRIV);
            break;
        case KW_CONST:
            if (!FuParser_check_fn(p)) {
                return FuParser_parse_item_const(p, attrs, VIS_PRIV);
            }
            break;
        case KW_LET:
            return FuParser_parse_item_let(p, attrs);
            break;
        case KW_BREAK:
            return FuParser_parse_item_break(p, attrs);
            break;
        case KW_CONTINUE:
            return FuParser_parse_item_continue(p, attrs);
            break;
        case KW_YIELD:
            return FuParser_parse_item_yield(p, attrs);
            break;
        case KW_THROW:
            return FuParser_parse_item_throw(p, attrs);
            break;
        case KW_RETURN:
            return FuParser_parse_item_return(p, attrs);
            break;
        case KW_IF:
            return FuParser_parse_item_if(p, attrs);
            break;
        case KW_MATCH:
            return FuParser_parse_item_match(p, attrs);
            break;
        case KW_TRY:
            return FuParser_parse_item_try(p, attrs);
            break;
        default:
            break;
        }
    }
    if (FuParser_check_fn(p)) {
        return FuParser_parse_item_fn(p, attrs, VIS_PRIV);
    }
    if (FuParser_check_loop(p, KW_FOR)) {
        return FuParser_parse_item_for(p, attrs);
    }
    if (FuParser_check_loop(p, KW_WHILE)) {
        return FuParser_parse_item_while(p, attrs);
    }
    if (FuParser_check_loop(p, KW_LOOP)) {
        return FuParser_parse_item_loop(p, attrs);
    }
    if (FuParser_check_block(p)) {
        return FuParser_parse_item_block(p, attrs);
    }
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    if (!FuParser_check_token_fn(p, FuToken_is_assign)) {
        FuParser_expect_token(p, TOK_SEMI);
        FuNode *nd = FuNode_new(p->ctx, expr->sp, ND_EXPR);
        nd->attrs = attrs;
        nd->_expr.expr = expr;
        return nd;
    }
    return FuParser_parse_item_assign(p, attrs, expr);
}

FuBlock *FuParser_parse_block(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *items = FuVec_new(sizeof(FuNode *));
    FuNode *item;
    while (1) {
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_CLOSE_BRACE) {
            break;
        }
        item = FuParser_parse_block_item(p);
        FuVec_push_ptr(items, item);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuBlock *blk = FuBlock_new(sp);
    blk->items = items;
    return blk;
}

static FuFnParam *FuParser_parse_fn_param(FuParser *p, fu_bool_t check_ty) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: parse attrs */
    FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
    FuType *ty;
    if (check_ty) {
        FuParser_expect_token(p, TOK_COLON);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        if (FuParser_check_token(p, TOK_COLON)) {
            FuParser_bump(p);
            ty = FuParser_parse_type(p, 0, FU_TRUE);
        } else {
            ty = FuType_from_keyword(p->ctx, pat->sp, KW_UNDERSCORE);
        }
    }
    FuFnParam *param = FuFnParam_new(pat->sp, pat);
    param->attrs = attrs;
    param->ty = ty;
    return param;
}

static FuVec *FuParser_parse_fn_params(FuParser *p, fu_token_k close_kd, fu_bool_t check_ty) {
    /* todo: parse type */
    FuVec *params = FuVec_new(sizeof(FuFnParam));
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == close_kd) {
        return params;
    }
    while (1) {
        FuFnParam *param = FuParser_parse_fn_param(p, check_ty);
        FuVec_push_ptr(params, param);
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_COMMA) {
            FuParser_bump(p);
            continue;
        }
        break;
    }
    return params;
}

static fu_bool_t FuParser_check_closure(FuParser *p) {
    FuToken tok;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd == TOK_EOF) {
            return FU_FALSE;
        }
        if (tok.kd == TOK_OR || tok.kd == TOK_OR_OR) {
            return FU_TRUE;
        }
        if (tok.kd == TOK_KEYWORD && (tok.sym == KW_ASYNC || tok.sym == KW_UNSAFE)) {
            i++;
            continue;
        }
        break;
    }
    return FU_FALSE;
}

static FuExpr *FuParser_parse_closure_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_async = FuParser_eat_keyword(p, KW_ASYNC);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    /* todo: parse closure type */
    FuVec *params;
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_OR_OR) {
        /* `|| expr` */
        params = FuVec_new(sizeof(FuFnParam *));
        FuParser_bump(p);
    } else {
        /* `| param ... |` expr */
        FuParser_expect_token(p, TOK_OR);
        params = FuParser_parse_fn_params(p, TOK_OR, FU_FALSE);
        FuParser_expect_token(p, TOK_OR);
    }
    FuType *return_ty;
    if (FuParser_check_token(p, TOK_RARROW)) {
        return_ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        return_ty = FuType_from_keyword(p->ctx, NULL, KW_UNDERSCORE);
    }
    FuFnSig *sig = FuFnSig_from_params(p->ctx, NULL, params, return_ty);
    FuExpr *body = FuParser_parse_expr(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(lo, body->sp);

    FuType *ty = FuType_new_fn_sig(p->ctx, sp, sig);
    FuExpr *expr = FuExpr_new(sp, EXPR_CLOSURE);
    expr->ty = ty;
    expr->_closure.is_async = is_async;
    expr->_closure.is_unsafe = is_unsafe;
    expr->_closure.params = params;
    expr->_closure.body = body;
    return expr;
}

static FuExpr *FuParser_parse_await_expr(FuParser *p) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_AWAIT);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    sp = FuSpan_join(tok.sp, expr->sp);
    FuExpr *await = FuExpr_new(sp, EXPR_AWAIT);
    await->_await.expr = expr;
    return await;
}

static FuExpr *FuParser_parse_if_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_IF);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuExpr *on_true = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuParser_expect_keyword(p, KW_ELSE);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuExpr *on_false = FuParser_parse_expr(p, 0, FU_TRUE);
    FuToken end = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_IF);
    expr->_if.cond = cond;
    expr->_if.on_true = on_true;
    expr->_if.on_false = on_false;
    return expr;
}

static FuExpr *FuParser_parse_keyword_expr(FuParser *p) {
    FuExpr *expr;
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        FATAL(tok.sp, "need keyword expr");
    }
    if (tok.sym == KW_NIL || tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
        FuLit *lit = FuParser_parse_lit(p);
        expr = FuExpr_new_lit(lit);
        return expr;
    }
    if (tok.sym == KW_SELF_LOWER || tok.sym == KW_SELF_UPPER) {
        FuPath *path = FuParser_parse_path(p);
        return FuExpr_new_path(NULL, path);
    }
    if (tok.sym == KW_AWAIT) {
        return FuParser_parse_await_expr(p);
    }
    if (tok.sym == KW_IF) {
        return FuParser_parse_if_expr(p);
    }
    if (tok.sym == KW_LET) {
        return FuParser_parse_let_cond_expr(p);
    }
    if (FuParser_check_closure(p)) {
        return FuParser_parse_closure_expr(p);
    }
    FATAL1(tok.sp, "invalid expr: `%s`", FuKind_keyword_cstr(tok.sym));
    return NULL;
}

FuExpr *FuParser_parse_expr(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null) {
    FuExpr *prefix_expr = NULL;

    /* parse prefix expr */
    FuToken tok = FuParser_nth_token(p, 0);
    switch (tok.kd) {
    case TOK_OPEN_PAREN:
        prefix_expr = FuParser_parse_group_or_tuple_expr(p);
        break;
    case TOK_BYTE:
    case TOK_CHAR:
    case TOK_INT:
    case TOK_FLOAT:
    case TOK_STR:
    case TOK_RAW_STR:
    case TOK_BYTE_STR:
    case TOK_BYTE_RAW_STR:
    case TOK_FORMAT_STR:
    case TOK_FORMAT_RAW_STR: {
        FuLit *lit = FuParser_parse_lit(p);
        prefix_expr = FuExpr_new_lit(lit);
        break;
    }
    case TOK_KEYWORD: {
        prefix_expr = FuParser_parse_keyword_expr(p);
        break;
    }
    case TOK_OPEN_BRACKET:
        prefix_expr = FuParser_parse_array_expr(p);
        break;
    case TOK_LT:
        /* check first invoke, begin expr */
        /* todo: expr->_path.anno */
        FATAL1(tok.sp, "unimplemented expr: `%s`", FuToken_kind_csr(tok));
    case TOK_IDENT: {
        FuPath *path = FuParser_parse_path(p);
        prefix_expr = FuExpr_new_path(NULL, path);
        break;
    }
    case TOK_OR:
    case TOK_OR_OR:
        prefix_expr = FuParser_parse_closure_expr(p);
        break;
    case TOK_MACRO: {
        FATAL(tok.sp, "unimplemented macro expr");
        /*
        prefix_expr = FuParser_parse_prefix_macro(p);
        */
        break;
    }
    default: {
        fu_op_k prefix_op;
        if (!FuToken_to_prefix_op(tok, &prefix_op)) {
            if (check_null) {
                FATAL1(tok.sp, "expect expression, find `%s`", FuToken_kind_csr(tok));
            } else {
                return NULL;
            }
        }
        prefix_expr = FuParser_parse_prefix_expr(p, prefix_op, FuOp_precedence(prefix_op));
        break;
    }
    }

    /* parse infix ops */
    FuExpr *left = prefix_expr;
    while (1) {
        tok = FuParser_nth_token(p, 0);

        fu_op_k infix_op;
        if (FuToken_to_infix_op(tok, &infix_op)) {
            fu_op_prec_t infix_prec = FuOp_precedence(infix_op);
            if (prec < infix_prec) {
                left = FuParser_parse_infix_expr(p, left, infix_op, infix_prec);
                continue;
            }
        }

        /* must after infix_op because of range ops */
        fu_op_k suffix_op;
        if (FuToken_to_suffix_op(tok, &suffix_op) && prec < FuOp_precedence(suffix_op)) {
            left = FuParser_parse_suffix_expr(p, left, suffix_op);
            continue;
        }
        break;
    }
    return left;
}

fu_vis_k FuParser_parse_visibility(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return VIS_PRIV;
        ;
    }
    switch (tok.sym) {
    case KW_PUB:
        FuParser_bump(p);
        return VIS_PUB;
        break;
    case KW_PKG:
        FuParser_bump(p);
        return VIS_PKG;
        break;
    default:
        return VIS_PRIV;
        break;
    }
}

static fu_bool_t FuParser_check_use(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return FU_FALSE;
    }
    if (tok.sym == KW_PKG || tok.sym == KW_PUB) {
        tok = FuParser_nth_token(p, 1);
    }
    if (tok.kd == TOK_KEYWORD && tok.sym == KW_USE) {
        return FU_TRUE;
    }
    return FU_FALSE;
};

static FuUse *FuParser_parse_use_tree(FuParser *p) {
    FuUse *use;
    FuSpan *sp;
    FuSpan *last_sp;
    FuSpan *lo = FuParser_current_span(p);
    last_sp = lo;
    FuToken tok = FuParser_nth_token(p, 0);
    FuPath *prefix = NULL;
    if (FuToken_is_ident(tok)) {
        prefix = FuParser_parse_path(p);
        last_sp = prefix->sp;
    }
    tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_SEMI || tok.kd == TOK_COMMA || FuParser_check_keyword(p, KW_AS)) {
        FuIdent *alias = NULL;
        if (FuParser_check_keyword(p, KW_AS)) {
            FuParser_expect_keyword(p, KW_AS);
            alias = FuParser_parse_ident(p);
            last_sp = alias->sp;
        }
        sp = FuSpan_join(lo, last_sp);
        use = FuUse_new(prefix->sp, USE_SIMPLE, prefix);
        use->_simple.alias = alias;
        return use;
    }
    if (prefix) {
        FuParser_expect_token(p, TOK_MOD_SEP);
    }
    tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_MACRO) {
        last_sp = tok.sp;
        FuToken macro_tok = FuParser_expect_token(p, TOK_MACRO);
        FuIdent *macro_name = FuIdent_new(macro_tok.sp, macro_tok.sym);
        last_sp = macro_name->sp;
        FuIdent *alias = NULL;
        if (FuParser_check_keyword(p, KW_AS)) {
            FuParser_expect_keyword(p, KW_AS);
            FuToken alias_tok = FuParser_expect_token(p, TOK_MACRO);
            alias = FuIdent_new(alias_tok.sp, alias_tok.sym);
            last_sp = alias->sp;
        }
        sp = FuSpan_join(lo, last_sp);
        use = FuUse_new(sp, USE_MACRO, prefix);
        use->_macro.name = macro_name;
        use->_macro.alias = alias;
        return use;
    }
    if (tok.kd == TOK_STAR) {
        last_sp = tok.sp;
        FuParser_expect_token(p, TOK_STAR);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_NOT) {
            sp = FuSpan_join(lo, tok.sp);
            use = FuUse_new(sp, USE_GLOB_MACRO, prefix);
            return use;
        }
        sp = FuSpan_join(lo, last_sp);
        use = FuUse_new(sp, USE_GLOB, prefix);
        return use;
    }
    if (tok.kd != TOK_OPEN_BRACE) {
        FATAL1(tok.sp, "invalid use item token: `%s`", FuToken_kind_csr(tok));
    }
    if (!prefix) {
        FATAL(tok.sp, "nested use tree must have prefix");
    }
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *nested = FuVec_new(sizeof(FuVec *));
    while (1) {
        FuUse *item = FuParser_parse_use_tree(p);
        FuVec_push_ptr(nested, item);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_CLOSE_BRACE) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
    }
    tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    sp = FuSpan_join(lo, tok.sp);
    use = FuUse_new(sp, USE_NESTED, prefix);
    use->_nested = nested;
    return use;
}

FuNode *FuParser_parse_item_use(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_USE);
    FuUse *tree = FuParser_parse_use_tree(p);
    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_USE);
    nd->attrs = attrs;
    nd->_use.vis = vis;
    nd->_use.tree = tree;
    return nd;
}

FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_STATIC);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuExpr *expr = NULL;
    if (!FuParser_check_token(p, TOK_SEMI)) {
        FuParser_expect_token(p, TOK_EQ);
        expr = FuParser_parse_expr(p, 0, FU_FALSE);
    }
    FuToken end = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end.sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_STATIC);
    nd->attrs = attrs;
    nd->_static.vis = vis;
    nd->_static.ident = ident;
    nd->_static.ty = ty;
    nd->_static.init = expr;
    return nd;
}

FuNode *FuParser_parse_item_const(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_CONST);
    FuIdent *ident = FuParser_parse_ident(p);
    FuType *ty;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_bump(p);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        ty = FuType_from_keyword(p->ctx, ident->sp, KW_UNDERSCORE);
    }
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, expr->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_CONST);
    nd->attrs = attrs;
    nd->_const.vis = vis;
    nd->_const.ident = ident;
    nd->_const.ty = ty;
    nd->_const.init = expr;
    return nd;
}

FuNode *FuParser_parse_item_let(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_LET);
    FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
    FuType *ty = NULL;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_bump(p);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    }
    FuExpr *expr = NULL;
    if (FuParser_check_token(p, TOK_EQ)) {
        FuParser_bump(p);
        expr = FuParser_parse_expr(p, 0, FU_TRUE);
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_LET);
    nd->attrs = attrs;
    nd->_let.pat = pat;
    nd->_let.ty = ty;
    nd->_let.init = expr;
    return nd;
}

FuNode *FuParser_parse_item_assign(FuParser *p, FuVec *attrs, FuExpr *lexpr) {
    FuToken op_tok = FuParser_expect_token_fn(p, FuToken_is_assign, "assign operater");
    FuExpr *rexpr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lexpr->sp, rexpr->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_ASSIGN);
    nd->attrs = attrs;
    nd->_assign.lexpr = lexpr;
    if (!FuToken_to_assign_op(op_tok, &(nd->_assign.op))) {
        FATAL1(op_tok.sp, "expect assign, find `%s`", FuToken_kind_csr(op_tok));
    }
    nd->_assign.op_sp = op_tok.sp;
    nd->_assign.rexpr = rexpr;
    return nd;
}

FuNode *FuParser_parse_item_break(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_BREAK);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        sp = FuSpan_join(tok.sp, label->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_BREAK);
    nd->attrs = attrs;
    nd->_break.label = label;
    return nd;
}

FuNode *FuParser_parse_item_continue(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_CONTINUE);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        sp = FuSpan_join(tok.sp, label->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_CONTINUE);
    nd->attrs = attrs;
    nd->_continue.label = label;
    return nd;
}

FuNode *FuParser_parse_item_yield(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_YIELD);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (expr) {
        sp = FuSpan_join(tok.sp, expr->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_YIELD);
    nd->attrs = attrs;
    nd->_yield.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_throw(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_THROW);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (expr) {
        sp = FuSpan_join(tok.sp, FuParser_current_span(p));
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_THROW);
    nd->attrs = attrs;
    nd->_throw.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_return(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_RETURN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (expr) {
        sp = FuSpan_join(tok.sp, expr->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_RETURN);
    nd->attrs = attrs;
    nd->_return.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_block(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    FuBlock *blk = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, blk->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_BLOCK);
    nd->attrs = attrs;
    nd->_block.is_unsafe = is_unsafe;
    nd->_block.block = blk;
    return nd;
}

static FuNode *FuParser_parse_next_if_node(FuParser *p) {
    if (!FuParser_check_keyword(p, KW_ELSE)) {
        return NULL;
    }
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_ELSE);
    if (FuParser_check_keyword(p, KW_IF)) {
        return FuParser_parse_item_if(p, NULL);
    }
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_IF);
    nd->_if.cond = NULL;
    nd->_if.block = block;
    nd->_if.next_if = NULL;
    return nd;
}

FuNode *FuParser_parse_item_if(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_IF);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_IF);
    nd->attrs = attrs;
    nd->_if.cond = cond;
    nd->_if.block = block;
    nd->_if.next_if = FuParser_parse_next_if_node(p);
    return nd;
}

FuNode *FuParser_parse_item_match(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_MATCH);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *arms = FuVec_new(sizeof(FuArm *));
    while (1) {
        FuVec *attrs = FuVec_new(sizeof(FuAttr *));
        /* todo: parse attrs */
        FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
        FuExpr *guard = NULL;
        if (FuParser_check_keyword(p, KW_IF)) {
            FuParser_expect_keyword(p, KW_IF);
            guard = FuParser_parse_expr(p, 0, FU_TRUE);
        }
        FuParser_expect_token(p, TOK_FAT_ARROW);
        FuNode *body = FuParser_parse_block_item(p);
        FuSpan *arm_sp = FuSpan_join(pat->sp, body->sp);
        FuArm *arm = FuArm_new(arm_sp, ARM_MATCH);
        arm->sp = arm_sp;
        arm->attrs = attrs;
        arm->pat = pat;
        arm->guard = guard;
        arm->_match.body = body;
        FuVec_push_ptr(arms, arm);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
    }
    FuSpan *hi = FuParser_current_span(p);
    FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, hi);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_MATCH);
    nd->attrs = attrs;
    nd->_match.cond = cond;
    nd->_match.arms = arms;
    return nd;
}

FuNode *FuParser_parse_item_loop(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        FuParser_expect_token(p, TOK_COLON);
    }
    FuParser_expect_keyword(p, KW_LOOP);
    FuBlock *blk = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, blk->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_LOOP);
    nd->attrs = attrs;
    nd->_loop.label = label;
    nd->_loop.block = blk;
    return nd;
}

FuNode *FuParser_parse_item_while(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        FuParser_expect_token(p, TOK_COLON);
    }
    FuParser_expect_keyword(p, KW_WHILE);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_WHILE);
    nd->attrs = attrs;
    nd->_while.cond = cond;
    nd->_while.block = block;
    return nd;
}

FuNode *FuParser_parse_item_for(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        FuParser_expect_token(p, TOK_COLON);
    }
    FuParser_expect_keyword(p, KW_FOR);
    FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
    FuParser_expect_keyword(p, KW_IN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_FOR);
    nd->attrs = attrs;
    nd->_for.label = label;
    nd->_for.pat = pat;
    nd->_for.expr = expr;
    nd->_for.block = block;
    return nd;
}

FuNode *FuParser_parse_item_try(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_TRY);
    FuBlock *block = FuParser_parse_block(p);
    FuVec *arms = FuVec_new(sizeof(FuArm *));
    FuSpan *last_arm_sp = block->sp;
    while (1) {
        if (!FuParser_check_keyword(p, KW_CATCH)) {
            break;
        }
        FuVec *arm_attrs = FuVec_new(sizeof(FuArm *));
        /* todo: parse attrs */
        FuToken tok = FuParser_expect_keyword(p, KW_CATCH);
        FuPat *arm_pat = FuParser_parse_pat(p, 0, FU_FALSE);
        FuExpr *arm_guard = NULL;
        if (FuParser_check_keyword(p, KW_IF)) {
            arm_guard = FuParser_parse_expr(p, 0, FU_TRUE);
        }
        FuBlock *arm_block = FuParser_parse_block(p);
        FuSpan *arm_sp = FuSpan_join(tok.sp, arm_block->sp);
        FuArm *arm = FuArm_new(arm_sp, ARM_CATCH);
        arm->attrs = arm_attrs;
        arm->pat = arm_pat;
        arm->guard = arm_guard;
        arm->_catch.body = arm_block;
        FuVec_push_ptr(arms, arm);
        last_arm_sp = arm_sp;
    }
    FuSpan *sp;
    FuBlock *finally = NULL;
    if (FuParser_check_keyword(p, KW_FINALLY)) {
        FuParser_expect_keyword(p, KW_FINALLY);
        finally = FuParser_parse_block(p);
        sp = FuSpan_join(lo, finally->sp);
    } else {
        sp = FuSpan_join(lo, last_arm_sp);
    }
    FuNode *nd = FuNode_new(p->ctx, sp, ND_TRY);
    nd->attrs = attrs;
    nd->_try.block = block;
    nd->_try.arms = arms;
    nd->_try.finally = finally;
    return nd;
}

FuNode *FuParser_parse_item_fn(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_const = FuParser_eat_keyword(p, KW_CONST);
    fu_bool_t is_async = FuParser_eat_keyword(p, KW_ASYNC);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    if (is_const && is_async) {
        ERROR(lo, "fn can not be `const` and `async` at the same time");
    }
    FuParser_expect_keyword(p, KW_FN);
    FuIdent *ident = FuParser_parse_ident(p);

    FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuVec *params = FuParser_parse_fn_params(p, TOK_CLOSE_PAREN, FU_TRUE);
    FuParser_expect_token(p, TOK_CLOSE_PAREN);

    FuType *return_ty;
    if (FuParser_check_token(p, TOK_RARROW)) {
        FuParser_expect_token(p, TOK_RARROW);
        return_ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        return_ty = FuType_from_keyword(p->ctx, NULL, KW_NIL);
    }
    FuFnSig *sig = FuFnSig_from_params(p->ctx, NULL, params, return_ty);
    sig->is_const = is_const;
    sig->is_async = is_async;
    sig->is_unsafe = is_unsafe;

    FuSpan *hi;
    FuBlock *body = NULL;
    if (FuParser_check_token(p, TOK_SEMI)) {
        FuToken tok = FuParser_expect_token(p, TOK_SEMI);
        hi = tok.sp;
    } else if (FuParser_check_token(p, TOK_OPEN_BRACE)) {
        body = FuParser_parse_block(p);
        hi = body->sp;
    } else {
        FuToken tok = FuParser_nth_token(p, 1);
        FATAL1(tok.sp, "expect `{` or `;`, find: `%s`", FuToken_kind_csr(tok));
    }

    FuSpan *sp = FuSpan_join(lo, hi);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_FN);
    nd->attrs = attrs;
    nd->_fn.vis = vis;
    nd->_fn.ident = ident;
    nd->_fn.params = params;
    nd->_fn.sig = sig;
    nd->_fn.body = body;
    return nd;
}

FuNode *FuParser_parse_mod_item(FuParser *p) {
    /* todo: parse attrs */
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    fu_vis_k vis = FuParser_parse_visibility(p);
    FuToken tok = FuParser_nth_token(p, 0);
    FuNode *item;
    if (tok.kd != TOK_KEYWORD) {
        /* todo: attr drop */
        FuVec_drop(attrs);
        FATAL1(tok.sp, "expect item keyword, find `%s`", FuToken_kind_csr(tok));
    }
    switch (tok.sym) {
    case KW_STATIC:
        item = FuParser_parse_item_static(p, attrs, vis);
        break;
    case KW_CONST:
        if (FuParser_check_fn(p)) {
            item = FuParser_parse_item_fn(p, attrs, vis);
            break;
        }
        item = FuParser_parse_item_const(p, attrs, vis);
        break;
    default: {
        if (FuParser_check_fn(p)) {
            item = FuParser_parse_item_fn(p, attrs, vis);
            break;
        }
        if (FuParser_check_use(p)) {
            item = FuParser_parse_item_use(p, attrs, vis);
            break;
        }
        FATAL1(tok.sp, "unimplement item: `%s`", FuKind_keyword_cstr(tok.sym));
        item = NULL;
        break;
    }
    }
    return item;
}

FuVec *FuParser_parse_mod_items(FuParser *p) {
    FuVec *items = FuVec_new(sizeof(FuNode *));
    while (!FuParser_is_eof(p)) {
        FuNode *item = FuParser_parse_mod_item(p);
        FuVec_push_ptr(items, item);
    }
    return items;
}

FuNode *FuParser_parse_pkg(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* FuParser_parse_inner_attrs(p, attrs); */
    FuVec *items = FuParser_parse_mod_items(p);
    FuSpan *hi = FuParser_current_span(p);
    FuSpan *sp = FuSpan_join(lo, hi);
    FuNode *nd = FuNode_new_pkg(p->ctx, sp);
    nd->attrs = attrs;
    nd->_pkg.name = KW_DOLLAR_PKG;
    nd->_pkg.items = items;
    return nd;
}

FuStr *FuParser_dump_tokens(FuParser *p) {
    FuStr *dump = FuStr_new();
    while (!FuParser_is_eof(p)) {
        FuToken tok = FuParser_nth_token(p, 0);
        FuStr *tok_str = FuToken_display(tok);
        FuStr_append(dump, FuSpan_display(tok.sp));
        FuStr_push_utf8_cstr(dump, ":");
        FuStr_append(dump, tok_str);
        FuStr_push_utf8_cstr(dump, "\n");
        FuParser_bump(p);
    }
    return dump;
}

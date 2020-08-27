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

/*
  关键字 nil, true, false 的 kd 属于 TOK_IDENT，
  这样在宏里面这些关键字可以统一作为 TOK_IDENT 类型
 */
FuIdent *FuParser_parse_ident(FuParser *p) {
    FuSpan *sp = FuParser_current_span(p);
    FuToken tok = FuParser_expect_token_fn(p, FuToken_is_ident, "expect ident");
    FuIdent *ident = FuMem_new(FuIdent);
    ident->sp = sp;
    ident->name = tok.sym;
    return ident;
}

FuPathItem *FuParser_parse_path_item(FuParser *p) {
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
    default:
        FATAL1(tok.sp, "invalid type: `%s`", FuToken_kind_csr(tok));
        break;
    }
    return NULL;
}

static FuType *FuParser_parse_path_type(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!FuParser_check_token_fn(p, FuToken_is_ident)) {
        FATAL1(tok.sp, "expect ident, find: `%s`", FuToken_kind_csr(tok));
    }
    FuPath *path = FuParser_parse_path(p);
    return FuType_new_path(p->ctx, path);
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

static FuExpr *FuParser_parser_dot_expr(FuParser *p, FuExpr *left) {
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

static FuExpr *FuParser_parser_array_expr(FuParser *p) {
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
        return FuParser_parser_dot_expr(p, left);
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

static FuExpr *FuParser_parse_group_or_tuple_expr(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuToken tok = FuParser_bump(p);

    /* group expression */
    if (tok.kd == TOK_CLOSE_PAREN) {
        return expr;
    }

    /* tuple expression */
    if (tok.kd == TOK_COMMA) {
        FuVec *fields = FuVec_new(sizeof(FuExpr *));
        while (tok.kd == TOK_COMMA) {
            FuVec_push_ptr(fields, expr);
            if (tok.kd != TOK_COMMA) {
                break;
            }
            expr = FuParser_parse_expr(p, 0, FU_TRUE);
            tok = FuParser_bump(p);
        }
        if (tok.kd != TOK_CLOSE_PAREN) {
            FATAL1(tok.sp, "expect `)`, find `%s`", FuToken_kind_csr(tok));
        }
        FuSpan *sp = FuSpan_join(open_tok.sp, tok.sp);
        FuExpr *tuple_expr = FuExpr_new(sp, EXPR_TUPLE);
        tuple_expr->_tuple.fields = fields;
        return tuple_expr;
    }
    FATAL1(tok.sp, "expect `)`, find tok: `%s`", FuToken_kind_csr(tok));
    return NULL;
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

static fu_bool_t FuParser_check_loop(FuParser *p) {
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
    if (tok.kd == TOK_KEYWORD && tok.sym == KW_LOOP) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static FuNode *FuParser_parse_item_block_keyword(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuToken tok = FuParser_nth_token(p, 0);
    switch (tok.sym) {
    case KW_STATIC:
        return FuParser_parse_item_static(p, attrs, vis);
        break;
    case KW_CONST:
        return FuParser_parse_item_const(p, attrs, vis);
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
    case KW_AWAIT:
        return FuParser_parse_item_await(p, attrs);
        break;
    case KW_RETURN:
        return FuParser_parse_item_return(p, attrs);
        break;
    case KW_UNSAFE:
    case KW_ASYNC:
        if (FuParser_check_block(p)) {
            return FuParser_parse_item_block(p, attrs);
        }
        break;
    case KW_LOOP:
        return FuParser_parse_item_loop(p, attrs);
        break;
    case KW_IF:
        return FuParser_parse_item_if(p, attrs);
        break;
    default:
        FATAL1(tok.sp, "unimplement item: `%s`", FuKind_keyword_cstr(tok.sym));
        break;
    }
    return NULL;
}

static FuNode *FuParser_parse_block_item(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: attrs */
    FuToken tok = FuParser_nth_token(p, 0);
    switch (tok.kd) {
    case TOK_KEYWORD:
        return FuParser_parse_item_block_keyword(p, attrs, VIS_PRIV);
        break;
    case TOK_LABEL:
    case TOK_OPEN_BRACE:
        if (FuParser_check_block(p)) {
            return FuParser_parse_item_block(p, attrs);
        }
        if (FuParser_check_loop(p)) {
            return FuParser_parse_item_loop(p, attrs);
        }
        FATAL1(tok.sp, "umimplement block item: `%s`", FuToken_kind_csr(tok));
        break;
    default: {
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
        if (FuParser_check_token_fn(p, FuToken_is_assign)) {
            return FuParser_parse_item_assign(p, attrs, expr);
        }
        FuNode *nd = FuNode_new(p->ctx, expr->sp, ND_EXPR);
        nd->attrs = attrs;
        nd->_expr.expr = expr;
        tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_SEMI) {
            if (FuExpr_can_endwith_semi(expr)) {
                nd->_expr.end_semi = FU_TRUE;
                FuParser_bump(p);
            } else {
                FATAL(tok.sp, "can not insert semicolon after this expression");
            }
        }
        return nd;
        break;
    }
    }
    return NULL;
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
    /* check expr end semi */
    fu_size_t len = FuVec_len(items);
    if (len > 1) {
        fu_size_t i;
        for (i = 0; i < len - 1; i++) {
            item = FuVec_get_ptr(items, i);
            if (item->kd != ND_EXPR) {
                continue;
            }
            if (FuExpr_can_endwith_semi(item->_expr.expr) && !item->_expr.end_semi) {
                FATAL(item->sp, "expression should end with semicolon");
            }
        }
    }

    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuBlock *blk = FuBlock_new(sp);
    blk->items = items;
    return blk;
}

FuExpr *FuParser_parse_block_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_async = FuParser_eat_keyword(p, KW_ASYNC);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        FuParser_expect_token(p, TOK_COLON);
    }
    FuBlock *blk = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, blk->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_BLOCK);
    expr->_block.is_async = is_async;
    expr->_block.is_unsafe = is_unsafe;
    expr->_block.label = label;
    expr->_block.block = blk;
    return expr;
}

FuFnParam *FuParser_parse_fn_param(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: parse attrs */
    FuIdent *ident = FuParser_parse_ident(p);
    FuType *ty;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_bump(p);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        ty = FuType_from_keyword(p->ctx, ident->sp, KW_UNDERSCORE);
    }
    FuFnParam *param = FuFnParam_new(ident->sp, ident);
    param->attrs = attrs;
    param->ty = ty;
    return param;
}

FuVec *FuParser_parse_fn_params(FuParser *p, fu_token_k close_kd) {
    /* todo: parse type */
    FuVec *params = FuVec_new(sizeof(FuFnParam));
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == close_kd) {
        return params;
    }
    while (1) {
        FuFnParam *param = FuParser_parse_fn_param(p);
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

FuExpr *FuParser_parse_closure_expr(FuParser *p) {
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
        params = FuParser_parse_fn_params(p, TOK_OR);
        FuParser_expect_token(p, TOK_OR);
    }
    FuType *return_ty;
    if (FuParser_check_token(p, TOK_RARROW)) {
        return_ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        return_ty = FuType_from_keyword(p->ctx, NULL, KW_UNDERSCORE);
    }

    FuExpr *body = FuParser_parse_expr(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(lo, body->sp);

    FuVec *sig_tys = FuVec_new(sizeof(FuType *));
    fu_size_t len = FuVec_len(params);
    fu_size_t i;
    for (i = 0; i < len; i++) {
        FuFnParam *param = FuVec_get_ptr(params, i);
        FuVec_push_ptr(sig_tys, param->ty);
    }
    if (len == 0) {
        FuVec_push_ptr(sig_tys, FuType_from_keyword(p->ctx, NULL, KW_NIL));
    }
    FuVec_push_ptr(sig_tys, return_ty);
    FuFnSig *sig = FuFnSig_new(NULL, sig_tys);

    FuType *ty = FuType_new_fn_sig(p->ctx, sp, sig);
    FuExpr *expr = FuExpr_new(sp, EXPR_CLOSURE);
    expr->ty = ty;
    expr->_closure.is_async = is_async;
    expr->_closure.is_unsafe = is_unsafe;
    expr->_closure.params = params;
    expr->_closure.body = body;
    return expr;
}

static FuExpr *FuParser_parse_loop_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuLabel *label = FuParser_parse_label(p);
    if (label) {
        FuParser_expect_token(p, TOK_COLON);
    }
    FuParser_expect_keyword(p, KW_LOOP);
    FuBlock *blk = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, blk->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_LOOP);
    expr->_loop.label = label;
    expr->_loop.block = blk;
    return expr;
}

static FuExpr *FuParser_parse_next_if_expr(FuParser *p) {
    if (!FuParser_check_keyword(p, KW_ELSE)) {
        return NULL;
    }
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_ELSE);
    FuExpr *cond = NULL;
    if (FuParser_check_keyword(p, KW_IF)) {
        FuParser_expect_keyword(p, KW_IF);
        cond = FuParser_parse_expr(p, 0, FU_TRUE);
    }
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_IF);
    expr->_if.cond = cond;
    expr->_if.block = block;
    if (cond) {
        expr->_if.next_if = FuParser_parse_next_if_expr(p);
    }
    return expr;
}

static FuExpr *FuParser_parse_if_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_IF);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_IF);
    expr->_if.cond = cond;
    expr->_if.block = block;
    expr->_if.next_if = FuParser_parse_next_if_expr(p);
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
    if (tok.sym == KW_IF) {
        return FuParser_parse_if_expr(p);
    }
    if (FuParser_check_block(p)) {
        return FuParser_parse_block_expr(p);
    }
    if (FuParser_check_closure(p)) {
        return FuParser_parse_closure_expr(p);
    }
    if (FuParser_check_loop(p)) {
        return FuParser_parse_loop_expr(p);
    }
    FATAL1(tok.sp, "unimplemented keyword expr: `%s`", FuKind_keyword_cstr(tok.sym));
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
        prefix_expr = FuParser_parser_array_expr(p);
        break;
    case TOK_LT:
        /* check first invoke, begin expr */
        /* todo: expr->_path.anno */
        FATAL1(tok.sp, "unimplemented expr: `%s`", FuToken_kind_csr(tok));
    case TOK_IDENT: {
        FuPath *path = FuParser_parse_path(p);
        /* todo: macro parse */
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
    case TOK_LABEL:
    case TOK_OPEN_BRACE:
        if (FuParser_check_block(p)) {
            prefix_expr = FuParser_parse_block_expr(p);
        } else if (FuParser_check_loop(p)) {
            prefix_expr = FuParser_parse_loop_expr(p);
        } else {
            FATAL(tok.sp, "unimplemented expr");
        }
        break;
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

FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_STATIC);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, expr->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_STATIC);
    nd->attrs = attrs;
    nd->_static.vis = vis;
    nd->_static.ident = ident;
    nd->_static.ty = ty;
    nd->_static.init_expr = expr;
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
    nd->_const.init_expr = expr;
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
    FuToken tok = FuParser_expect_keyword(p, KW_BREAK);
    FuSpan *sp;
    FuLabel *label = FuParser_parse_label(p);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (expr) {
        sp = FuSpan_join(tok.sp, expr->sp);
    } else if (label) {
        sp = FuSpan_join(tok.sp, label->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_BREAK);
    nd->attrs = attrs;
    nd->_break.label = label;
    nd->_break.expr = expr;
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
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    sp = FuSpan_join(tok.sp, expr->sp);
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

FuNode *FuParser_parse_item_await(FuParser *p, FuVec *attrs) {
    FuSpan *sp;
    FuToken tok = FuParser_expect_keyword(p, KW_AWAIT);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    sp = FuSpan_join(tok.sp, expr->sp);
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_AWAIT);
    nd->attrs = attrs;
    nd->_await.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_return(FuParser *p, FuVec *attrs) {
    FuToken tok = FuParser_expect_keyword(p, KW_RETURN);
    FuSpan *sp;
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
    FuExpr *expr = FuParser_parse_block_expr(p);
    FuNode *nd = FuNode_new(p->ctx, expr->sp, ND_EXPR);
    nd->attrs = attrs;
    nd->_expr.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_loop(FuParser *p, FuVec *attrs) {
    FuExpr *expr = FuParser_parse_loop_expr(p);
    FuNode *nd = FuNode_new(p->ctx, expr->sp, ND_EXPR);
    nd->attrs = attrs;
    nd->_expr.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_if(FuParser *p, FuVec *attrs) {
    FuExpr *expr = FuParser_parse_if_expr(p);
    /* todo: check if/else */
    FuNode *nd = FuNode_new(p->ctx, expr->sp, ND_EXPR);
    nd->attrs = attrs;
    nd->_expr.expr = expr;
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
        item = FuParser_parse_item_const(p, attrs, vis);
        break;
    default:
        FATAL1(tok.sp, "unimplement item: `%s`", FuKind_keyword_cstr(tok.sym));
        item = NULL;
        break;
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

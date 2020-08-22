#include <assert.h>
#include <stdarg.h>

#include "alloc.h"
#include "log.h"
#include "parse.h"

FuParser *FuParser_new(FuCtx *ctx) {
    FuParser *p = FuMem_new(FuParser);
    p->ctx = ctx;
    p->tok_buf = FuVec_new(sizeof(FuToken));
    p->cursor = 0;
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
        } while (tok0.kd == TOK_COMMENT || tok0.kd == TOK_WHITESPACE || tok0.kd == TOK_NEWLINE);
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
    fu_size_t i = p->cursor + n;
    while (i >= FuVec_len(p->tok_buf)) {
        FuToken tok = FuParser_get_token(p);
        FuVec_push(p->tok_buf, &tok);
    }
    return *(FuToken *)FuVec_get(p->tok_buf, i);
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
    FuVec_remove_slice(p->tok_buf, 0, p->cursor + 1, NULL);
    FuToken tok = FuParser_get_token(p);
    FuVec_push(p->tok_buf, &tok);
    return cur_tok;
}

static FuSpan *FuParser_current_span(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    return tok.sp;
}

static FuToken FuParser_expect_keyword(FuParser *p, fu_keyword_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        FATAL2(tok.sp, "expect %s, find token: %s", FuKind_keyword_cstr(kd), FuKind_token_cstr(tok.kd));
    }
    if (tok.sym != kd) {
        FATAL2(tok.sp, "expect %s, find keyword: %s", FuKind_keyword_cstr(kd), FuKind_keyword_cstr(tok.sym));
    }
    return FuParser_bump(p);
}

static FuToken FuParser_expect_token(FuParser *p, fu_token_k kd) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != kd) {
        FATAL2(tok.sp, "expect token: %s, find token: %s", FuKind_token_cstr(kd), FuKind_token_cstr(tok.kd));
    }
    return FuParser_bump(p);
}

static FuToken FuParser_expect_token_fn(FuParser *p, FuCheckTokenFn fn, char *wanted) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!fn(tok)) {
        FATAL2(tok.sp, "expect %s, find token: %s", wanted, FuKind_token_cstr(tok.kd));
    }
    return FuParser_bump(p);
}

static fu_bool_t FuParser_check_token(FuParser *p, fu_token_k kd) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == kd) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

/*
static fu_bool_t FuParser_check_token_fn(FuParser *p, FuCheckTokenFn fn) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (fn(tok)) {
        return FU_TRUE;
    }
    return FU_FALSE;
}
*/

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
            FATAL(tok.sp, "expect literal");
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
        FATAL1(tok.sp, "unimplemented: %s", FuKind_token_cstr(tok.kd));
        break;
    default:
        FATAL(tok.sp, "can not be here");
    }
    return lit;
}

static FuVec *FuParser_parse_fn_args(FuParser *p) {
    FuVec *args = FuVec_new(sizeof(FuExpr *));
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
    if (!expr) {
        return args;
    }
    FuVec_push_ptr(args, expr);
    FuToken tok = FuParser_nth_token(p, 0);
    while (tok.kd == TOK_COMMA) {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
        FuVec_push_ptr(args, expr);
        tok = FuParser_nth_token(p, 0);
    }
    return args;
}

static FuExpr *FuParser_parse_call_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuVec *args = FuParser_parse_fn_args(p);
    FuToken tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    if (left->kd == EXPR_FIELD) {
        FuExpr *expr = FuExpr_new(sp, EXPR_METHOD_CALL);
        expr->_method_call.base = left;
        expr->_method_call.args = args;
        return expr;
    }
    /* todo: tuple struct */
    FuExpr *expr = FuExpr_new(sp, EXPR_CALL);
    expr->_call.base = left;
    expr->_call.args = args;
    return expr;
}

static FuExpr *FuParser_parser_field_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_DOT);
    FuPathItem *field = FuParser_parse_path_item(p);
    FuSpan *sp = FuSpan_join(field->sp, left->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_FIELD);
    expr->_field.base = left;
    expr->_field.field = field;
    return expr;
}

static FuExpr *FuParser_parse_index_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuToken tok;
    FuExpr *idx = FuParser_parse_expr(p, 0, FU_TRUE);
    tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_INDEX);
    expr->_index.obj = left;
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
        FATAL1(tok1.sp, "expect identifier name or array index, but find token: %s", FuKind_token_cstr(tok1.kd));
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
    FuFieldInit *init = FuParser_parse_field_init(p);
    FuVec_push_ptr(inits, init);
    tok = FuParser_nth_token(p, 0);
    while (tok.kd == TOK_COMMA) {
        FuParser_bump(p);
        init = FuParser_parse_field_init(p);
        FuVec_push_ptr(inits, init);
        tok = FuParser_nth_token(p, 0);
    }
    return inits;
}

static FuExpr *FuParser_parse_struct_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *inits = FuParser_parse_field_inits(p);
    FuToken tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_STRUCT);
    expr->_struct.base = left;
    expr->_struct.field_inits = inits;
    return expr;
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
    case OP_FIELD:
        return FuParser_parser_field_expr(p, left);
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
    case OP_ASSIGN:
    case OP_ADD_ASSIGN:
    case OP_SUB_ASSIGN:
    case OP_MUL_ASSIGN:
    case OP_DIV_ASSIGN:
    case OP_REM_ASSIGN:
    case OP_BIT_AND_ASSIGN:
    case OP_BIT_OR_ASSIGN:
    case OP_BIT_XOR_ASSIGN:
    case OP_SHL_ASSIGN:
    case OP_SHR_ASSIGN: {
        FATAL1(op_tok.sp, "unimplemented op: ", FuKind_op_cstr(op));
        break;
    }
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
        FuVec_push_ptr(fields, expr);
        while (tok.kd == TOK_COMMA) {
            expr = FuParser_parse_expr(p, 0, FU_TRUE);
            FuVec_push_ptr(fields, expr);
            tok = FuParser_bump(p);
        }
        if (tok.kd != TOK_CLOSE_PAREN) {
            FATAL1(tok.sp, "expect `)`, find token: ", FuKind_token_cstr(tok.kd));
        }
        FuSpan *sp = FuSpan_join(open_tok.sp, tok.sp);
        FuExpr *tuple_expr = FuExpr_new(sp, EXPR_TUPLE);
        tuple_expr->_tuple.fields = fields;
        return tuple_expr;
    }
    FATAL1(tok.sp, "expect ), but found tok: %s", FuKind_token_cstr(tok.kd));
    return NULL;
}

static FuLabel *FuParser_parse_label(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_LABEL) {
        return NULL;
    }
    FuParser_bump(p);
    return FuLabel_new(tok.sp, tok.sym);
}

static FuExpr *FuParser_parse_keyword_expr(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    assert(tok.kd == TOK_KEYWORD);
    FuSpan *sp;
    FuExpr *keyword_expr;
    switch (tok.sym) {
    case KW_RETURN: {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
        if (expr) {
            sp = FuSpan_join(tok.sp, expr->sp);
        } else {
            sp = tok.sp;
        }
        keyword_expr = FuExpr_new(sp, EXPR_RETURN);
        keyword_expr->_return.expr = expr;
        return keyword_expr;
        break;
    }
    case KW_BREAK: {
        FuParser_bump(p);
        FuLabel *label = FuParser_parse_label(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
        if (expr) {
            sp = FuSpan_join(tok.sp, expr->sp);
        } else if (label) {
            sp = FuSpan_join(tok.sp, label->sp);
        } else {
            sp = tok.sp;
        }
        keyword_expr = FuExpr_new(sp, EXPR_BREAK);
        keyword_expr->_break.label = label;
        keyword_expr->_break.expr = expr;
        return keyword_expr;
        break;
    }
    case KW_CONTINUE: {
        FuParser_bump(p);
        FuLabel *label = FuParser_parse_label(p);
        if (label) {
            sp = FuSpan_join(tok.sp, label->sp);
        } else {
            sp = tok.sp;
        }
        keyword_expr = FuExpr_new(sp, EXPR_CONTINUE);
        keyword_expr->_continue.label = label;
        return keyword_expr;
        break;
    }
    case KW_YIELD: {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
        sp = FuSpan_join(tok.sp, expr->sp);
        keyword_expr = FuExpr_new(sp, EXPR_YIELD);
        keyword_expr->_yield.expr = expr;
        return keyword_expr;
        break;
    }
    case KW_AWAIT: {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
        sp = FuSpan_join(tok.sp, expr->sp);
        keyword_expr = FuExpr_new(sp, EXPR_AWAIT);
        keyword_expr->_await.expr = expr;
        return keyword_expr;
        break;
    }
    case KW_THROW: {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE);
        if (expr) {
            sp = FuSpan_join(tok.sp, FuParser_current_span(p));
        } else {
            sp = tok.sp;
        }
        keyword_expr = FuExpr_new(sp, EXPR_THROW);
        keyword_expr->_throw.expr = expr;
        return keyword_expr;
        break;
    } break;
    case KW_IF:
    case KW_MATCH:
    case KW_WHILE:
    case KW_DO:
    case KW_FOR:
    case KW_LOOP:
    case KW_TRY:
    case KW_LET:
        FATAL1(tok.sp, "unimplemented expr %s", FuKind_token_cstr(tok.kd));
        break;
    default:
        break;
    }
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
        if (tok.sym == KW_NIL || tok.sym == KW_TRUE || tok.sym == KW_FALSE) {
            FuLit *lit = FuParser_parse_lit(p);
            prefix_expr = FuExpr_new_lit(lit);
        } else {
            /* other keyword */
            prefix_expr = FuParser_parse_keyword_expr(p);
        }
        break;
    }
    case TOK_OPEN_BRACKET:
        prefix_expr = FuParser_parser_array_expr(p);
        break;
    case TOK_LT:
        /* check first invoke, begin expr */
        /* todo: expr->_path.anno */
        FATAL1(tok.sp, "unimplemented expr: %s", FuKind_token_cstr(tok.kd));
    case TOK_IDENT: {
        FuPath *path = FuParser_parse_path(p);
        /* todo: macro parse */
        prefix_expr = FuExpr_new_path(NULL, path);
        break;
    }
    case TOK_OR:
        FATAL(tok.sp, "unimplemented closure expr");
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
                FATAL1(tok.sp, "expect expression, find token: %s", FuKind_token_cstr(tok.kd));
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
        if (FuParser_check_token(p, TOK_MOD_SEP)) {
            FuParser_bump(p);
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

fu_vis_k FuParser_parse_visibility(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return VIS_PRI;
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
        return VIS_PRI;
        break;
    }
}

FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_STATIC);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, expr->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_STATIC);
    nd->attrs = attrs;
    nd->_static.vis = vis;
    nd->_static.ident = ident;
    nd->_static.init_expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_const(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_CONST);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, expr->sp);
    FuNode *nd = FuNode_new(p->ctx, sp, ND_CONST);
    nd->attrs = attrs;
    nd->_const.vis = vis;
    nd->_const.ident = ident;
    nd->_const.init_expr = expr;
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
        FATAL1(tok.sp, "expect item keyword, find tok: %s", FuKind_token_cstr(tok.kd));
    }
    switch (tok.sym) {
    case KW_STATIC:
        item = FuParser_parse_item_static(p, attrs, vis);
        break;
    case KW_CONST:
        item = FuParser_parse_item_const(p, attrs, vis);
        break;
    default:
        FATAL1(tok.sp, "unimplement item: %s", FuKind_keyword_cstr(tok.sym));
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

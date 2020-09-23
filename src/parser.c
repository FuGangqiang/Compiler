#include <assert.h>
#include <stdarg.h>

#include "alloc.h"
#include "log.h"
#include "parse.h"
#include "unix.h"

void FuParserState_drop(FuParserState *state) {
    if (!state) {
        return;
    }
    FuStr_drop(state->cur_dir);
    FuLexer_drop(state->lexer);
    FuVec_drop(state->tok_buf);
    FuMem_free(state);
}

FuParser *FuParser_new(FuPkg *pkg) {
    FuParser *p = FuMem_new(FuParser);
    p->pkg = pkg;
    p->states = FuVec_new(sizeof(FuParserState *));
    return p;
}

void FuParser_drop(FuParser *p) {
    if (!p) {
        return;
    }
    FuStr_drop(p->cur_dir);
    FuVec_drop_with_ptrs(p->states, (FuDropFn)FuParserState_drop);
    FuVec_drop(p->tok_buf);
    FuLexer_drop(p->lexer);
    FuMem_free(p);
}

void FuParser_for_file(FuParser *p, FuStr *fpath) {
    p->lexer = FuLexer_new(p->pkg);
    p->tok_buf = FuVec_new(sizeof(FuToken));
    p->tok_level = TOK_LEVEL_OPS;
    FuLexer_for_file(p->lexer, fpath);
}

static void FuParser_unfor_file(FuParser *p) {
    FuVec_drop(p->tok_buf);
    FuLexer_drop(p->lexer);
}

static void FuParser_push_state(FuParser *p) {
    FuParserState *state = FuMem_new(FuParserState);
    state->cur_dir = p->cur_dir;
    state->lexer = p->lexer;
    state->tok_buf = p->tok_buf;
    state->tok_level = p->tok_level;
    FuVec_push_ptr(p->states, state);
}

static fu_bool_t FuParser_pop_state(FuParser *p) {
    if (FuVec_len(p->states) == 0) {
        return FU_FALSE;
    }
    FuParserState *state;
    FuVec_pop_ptr(p->states, (void **)&state);
    p->tok_level = state->tok_level;
    p->tok_buf = state->tok_buf;
    p->lexer = state->lexer;
    p->cur_dir = state->cur_dir;
    FuMem_free(state);
    return FU_TRUE;
}

static FuToken FuParser_convert_raw_ident(FuParser *p, FuToken tok0) {
    FuToken tok1 = FuLexer_get_token(p->lexer);
    if (tok1.kd == TOK_NOT) {
        FuSpan *sp = FuSpan_join(tok0.sp, tok1.sp);
        FuStr *name = FuStr_clone(FuPkg_get_symbol(p->pkg, tok0.sym));
        FuStr_push(name, '!');
        fu_sym_t sym = FuPkg_intern_symbol(p->pkg, name);
        return FuToken_new_macro(sp, sym);
    } else {
        FuLexer_unget_token(p->lexer, tok1);
    }
    return FuToken_new_ident(tok0.sp, tok0.sym);
}

static FuToken FuParser_convert_macro_keyword_ident(FuParser *p, FuToken tok0) {
    if (tok0.sym < _KW_LAST_UNUSED) {
        return FuToken_new_keyword(tok0.sp, tok0.sym);
    }
    FuToken tok1 = FuLexer_get_token(p->lexer);
    if (tok1.kd == TOK_NOT) {
        FuSpan *sp = FuSpan_join(tok0.sp, tok1.sp);
        FuStr *name = FuStr_clone(FuPkg_get_symbol(p->pkg, tok0.sym));
        FuStr_push(name, '!');
        fu_sym_t sym = FuPkg_intern_symbol(p->pkg, name);
        return FuToken_new_macro(sp, sym);
    } else {
        FuLexer_unget_token(p->lexer, tok1);
    }
    return tok0;
}

static FuToken FuParser_get_token(FuParser *p) {
    FuToken tok0, tok1, tok2;
    FuSpan *sp;

    tok0 = FuLexer_get_token(p->lexer);

    if (p->tok_level >= TOK_LEVEL_NO_BLANK) {
        while (FuToken_is_blank(tok0)) {
            tok0 = FuLexer_get_token(p->lexer);
        }
    }

    if (p->tok_level >= TOK_LEVEL_IDENT) {
        if (tok0.kd == TOK_RAW_IDENT) {
            tok0 = FuParser_convert_raw_ident(p, tok0);
        }
        if (tok0.kd == TOK_IDENT) {
            tok0 = FuParser_convert_macro_keyword_ident(p, tok0);
        }
    }

    if (p->tok_level >= TOK_LEVEL_GE) {
        /* generic ops: `->`, `::` */
        if (tok0.kd == TOK_MINUS) {
            tok1 = FuLexer_get_token(p->lexer);
            if (tok1.kd == TOK_GT) {
                /* `->` */
                sp = FuSpan_join(tok0.sp, tok1.sp);
                tok0 = FuToken_new(TOK_RARROW, sp);
            } else {
                /* `-` */
                FuLexer_unget_token(p->lexer, tok1);
            }
        }
        if (tok0.kd == TOK_COLON) {
            tok1 = FuLexer_get_token(p->lexer);
            if (tok1.kd == TOK_COLON) {
                /* `::` */
                sp = FuSpan_join(tok0.sp, tok1.sp);
                tok0 = FuToken_new(TOK_MOD_SEP, sp);
            } else {
                /* `:` */
                FuLexer_unget_token(p->lexer, tok1);
            }
        }
    }

    if (p->tok_level <= TOK_LEVEL_GE) {
        return tok0;
    }

    /* TOK_LEVEL_OPS */
    switch (tok0.kd) {
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
        /* `-`, `-=` */
        tok1 = FuLexer_get_token(p->lexer);
        if (tok1.kd == TOK_EQ) {
            /* `-=` */
            sp = FuSpan_join(tok0.sp, tok1.sp);
            return FuToken_new(TOK_MINUS_EQ, sp);
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
                FuPkg_intern_span(p->pkg, tmp_sp);
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
                FuPkg_intern_span(p->pkg, tmp_sp);
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
                FuPkg_intern_span(p->pkg, tmp_sp);
                return FuToken_new(TOK_DOT_DOT, tmp_sp);
            }
        } else {
            /* `.` */
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

static fu_bool_t FuParser_check_newline(FuParser *p) {
    fu_tok_level_t old_tok_level = p->tok_level;
    p->tok_level = TOK_LEVEL_RAW;
    fu_bool_t res = FU_FALSE;
    FuToken tok;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd == TOK_WHITESPACE) {
            i++;
            continue;
        }
        if (tok.kd == TOK_NEWLINE || tok.kd == TOK_COMMENT || tok.kd == TOK_DOC_COMMENT || tok.kd == TOK_EOF) {
            res = FU_TRUE;
            break;
        }
        break;
    }
    do {
        FuVec_pop(p->tok_buf, &tok);
        FuLexer_unget_token(p->lexer, tok);
        i--;
    } while (i == 0);
    p->tok_level = old_tok_level;
    return res;
}

static fu_bool_t FuParser_check_token(FuParser *p, fu_token_k kd) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == kd) {
        return FU_TRUE;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_2_token(FuParser *p, fu_token_k kd0, fu_token_k kd1) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == kd0) {
        FuToken tok1 = FuParser_nth_token(p, 1);
        if (tok1.kd == kd1) {
            return FU_TRUE;
        }
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
    ident->is_macro = tok.kd == TOK_MACRO ? FU_TRUE : FU_FALSE;
    ident->name = tok.sym;
    return ident;
}

FuLabel *FuParser_parse_label(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_LABEL) {
        return NULL;
    }
    FuParser_bump(p);
    return FuLabel_new(tok.sp, tok.sym);
}

static FuGeArg *FuParser_parse_ge_arg(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuGeArg *arg;
    if (FuParser_check_2_token(p, TOK_IDENT, TOK_EQ)) {
        FuIdent *param = FuParser_parse_ident(p);
        if (param->is_macro) {
            ERROR(param->sp, "generic arg param can not be macro ident");
        }
        FuParser_expect_token(p, TOK_EQ);
        if (FuParser_check_token_fn(p, FuToken_is_lit)) {
            /* `A=1` in `Foo<A=1>` */
            FuLit *lit = FuParser_parse_lit(p);
            FuSpan *sp = FuSpan_join(lo, lit->sp);
            arg = FuGeArg_new(sp, GE_ARG_BINDING_CONST);
            arg->_binding_const.param = param;
            arg->_binding_const.lit = lit;
            return arg;
        }
        /* `A = Bar` in `Foo<A = Bar>` */
        FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
        FuSpan *sp = FuSpan_join(lo, ty->sp);
        arg = FuGeArg_new(sp, GE_ARG_BINDING);
        arg->_binding.param = param;
        arg->_binding.ty = ty;
        return arg;
    }
    if (FuParser_check_token_fn(p, FuToken_is_lit)) {
        /* `1` in `Foo<1>` */
        FuLit *lit = FuParser_parse_lit(p);
        arg = FuGeArg_new(lit->sp, GE_ARG_CONST);
        arg->_const = lit;
        return arg;
    }
    if (FuParser_check_token_fn(p, FuToken_is_ident)) {
        /* `Bar` in `Foo<Bar>` */
        FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
        arg = FuGeArg_new(ty->sp, GE_ARG_TYPE);
        arg->_type = ty;
        return arg;
    }
    FATAL(lo, "expect generic argument");
    return NULL;
}

static FuVec *FuParser_parse_ge_args(FuParser *p, FuSpan **end_sp_p) {
    FuParser_expect_token(p, TOK_POUND);
    FuParser_expect_token(p, TOK_LT);
    FuVec *args = FuVec_new(sizeof(FuGeArg *));
    while (1) {
        FuGeArg *arg = FuParser_parse_ge_arg(p);
        FuVec_push_ptr(args, arg);
        if (FuParser_check_token(p, TOK_GT)) {
            break;
        }
        if (FuParser_check_token(p, TOK_COMMA)) {
            FuParser_expect_token(p, TOK_COMMA);
            continue;
        }
        break;
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_GT);
    *end_sp_p = end_tok.sp;
    return args;
}

static FuVec *FuParser_parse_type_param_bounds(FuParser *p) {
    FuVec *bounds = FuVec_new(sizeof(FuType *));
    while (1) {
        FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
        FuVec_push_ptr(bounds, ty);
        if (FuParser_check_token(p, TOK_PLUS)) {
            FuParser_expect_token(p, TOK_PLUS);
            continue;
        }
        break;
    }
    return bounds;
}

static void FuParser_parse_ge_param(FuParser *p, FuVec *params, FuVec *bounds) {
    FuSpan *lo = FuParser_current_span(p);
    if (FuParser_check_keyword(p, KW_CONST)) {
        FuParser_bump(p);
        FuIdent *ident = FuParser_parse_ident(p);
        FuParser_expect_token(p, TOK_COLON);
        FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
        FuSpan *sp = FuSpan_join(lo, ty->sp);
        FuGeParam *param = FuGeParam_new(sp, GE_PARAM_CONST);
        param->ident = ident;
        param->_const.ty = ty;
        FuVec_push_ptr(params, param);
        return;
    }
    FuSpan *hi;
    FuIdent *ident = FuParser_parse_ident(p);
    hi = ident->sp;
    FuVec *interfaces = NULL;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_bump(p);
        interfaces = FuParser_parse_type_param_bounds(p);
        FuType *last = FuVec_last_ptr(interfaces);
        hi = last->sp;
    }
    FuType *def = NULL;
    if (FuParser_check_token(p, TOK_EQ)) {
        FuParser_bump(p);
        def = FuParser_parse_type(p, 0, FU_TRUE);
        hi = def->sp;
    }
    FuSpan *sp = FuSpan_join(lo, hi);
    FuGeParam *param = FuGeParam_new(sp, GE_PARAM_TYPE);
    param->ident = ident;
    param->_type.def = def;
    FuVec_push_ptr(params, param);
    if (interfaces) {
        FuGeBound *bound = FuGeBound_new(FuIdent_clone(ident), interfaces);
        FuVec_push_ptr(bounds, bound);
    }
}

static void FuParser_parse_ge_bound(FuParser *p, FuVec *bounds) {
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuVec *interfaces = FuParser_parse_type_param_bounds(p);
    FuGeBound *bound = FuGeBound_new(ident, interfaces);
    FuVec_push_ptr(bounds, bound);
}

static FuGeneric *FuParser_parse_ge(FuParser *p) {
    fu_tok_level_t old_tok_level = p->tok_level;
    p->tok_level = TOK_LEVEL_GE;
    FuParser_expect_token(p, TOK_POUND);
    FuParser_expect_token(p, TOK_LT);
    FuVec *params = FuVec_new(sizeof(FuGeParam *));
    FuVec *bounds = FuVec_new(sizeof(FuGeBound *));
    while (1) {
        FuParser_parse_ge_param(p, params, bounds);
        if (FuParser_check_token(p, TOK_COMMA)) {
            FuParser_bump(p);
            continue;
        }
        break;
    }
    if (FuParser_check_keyword(p, KW_WHERE)) {
        while (1) {
            FuParser_parse_ge_bound(p, bounds);
            if (FuParser_check_token(p, TOK_COMMA)) {
                FuParser_bump(p);
                continue;
            }
            break;
        }
    }
    FuParser_expect_token(p, TOK_GT);
    p->tok_level = old_tok_level;
    return FuGeneric_new(params, bounds);
}

static FuPathItem *FuParser_parse_path_item(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuSpan *hi;
    FuIdent *ident = FuParser_parse_ident(p);
    hi = ident->sp;
    FuVec *ge_args = NULL;
    if (FuParser_check_2_token(p, TOK_POUND, TOK_LT)) {
        ge_args = FuParser_parse_ge_args(p, &hi);
    }
    FuPathItem *item = FuMem_new(FuPathItem);
    item->sp = FuSpan_join(lo, hi);
    item->ident = ident;
    item->ge_args = ge_args;
    return item;
}

FuPath *FuParser_parse_path(FuParser *p) {
    FuPath *path = FuMem_new(FuPath);
    path->segments = FuVec_new(sizeof(FuPathItem *));
    FuPathItem *item;
    while (1) {
        item = FuParser_parse_path_item(p);
        FuVec_push_ptr(path->segments, item);
        FuToken tok0 = FuParser_nth_token(p, 0);
        if (tok0.kd == TOK_MOD_SEP) {
            FuToken tok1 = FuParser_nth_token(p, 1);
            if (FuToken_is_ident(tok1)) {
                if (item->ident->is_macro) {
                    ERROR(item->sp, "macro ident only allow in last path item");
                }
                if (tok1.kd == TOK_KEYWORD) {
                    ERROR1(tok1.sp, "`%s` can only be prefix", FuToken_kind_csr(tok1));
                }
                FuParser_bump(p);
                continue;
            }
        }
        break;
    }
    FuPathItem *start = FuVec_first_ptr(path->segments);
    FuPathItem *end = FuVec_last_ptr(path->segments);
    path->sp = FuSpan_join(start->sp, end->sp);
    path->is_macro = item->ident->is_macro;
    return path;
}

FuAnno *FuParser_parse_anno(FuParser *p, FuPath **path_p) {
    fu_tok_level_t old_tok_level = p->tok_level;
    p->tok_level = TOK_LEVEL_GE;
    FuToken start_tok = FuParser_expect_token(p, TOK_LT);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuParser_expect_keyword(p, KW_AS);
    FuPath *path = FuParser_parse_path(p);
    FuToken end_tok = FuParser_expect_token(p, TOK_GT);
    fu_size_t idx = FuVec_len(path->segments);
    FuSpan *sp = FuSpan_join(start_tok.sp, end_tok.sp);
    FuAnno *anno = FuMem_new(FuAnno);
    anno->sp = sp;
    anno->ty = ty;
    anno->idx = idx;
    *path_p = path;
    p->tok_level = old_tok_level;
    return anno;
}

static FuType *FuParser_parse_path_type(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (!FuParser_check_token_fn(p, FuToken_is_ident)) {
        FATAL1(tok.sp, "expect ident, find: `%s`", FuToken_kind_csr(tok));
    }
    FuPath *path = FuParser_parse_path(p);
    if (path->is_macro) {
        ERROR(path->sp, "macro ident can not be allowed in type");
    }
    return FuType_new_path(p->pkg, NULL, path);
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
        return FuType_from_keyword(p->pkg, tok.sp, tok.sym);
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
    FuToken tok0 = FuParser_expect_token(p, TOK_STAR);
    if (FuParser_check_keyword(p, KW_DYN)) {
        FuParser_expect_keyword(p, KW_DYN);
        FuType *right = FuParser_parse_type(p, prec, FU_FALSE);
        FuSpan *sp = FuSpan_join(tok0.sp, right->sp);
        FuType *ty = FuType_new(p->pkg, sp, TY_DYN_PTR);
        ty->_dyn_ptr = right;
        return ty;
    }
    FuToken tok1 = FuParser_nth_token(p, 0);
    if (FuToken_is_ident(tok1) && tok1.sym == FuPkg_intern_cstr(p->pkg, "raw")) {
        FuParser_bump(p);
        FuType *right = FuParser_parse_type(p, prec, FU_FALSE);
        FuSpan *sp = FuSpan_join(tok0.sp, right->sp);
        FuType *ty = FuType_new(p->pkg, sp, TY_RAW_PTR);
        ty->_raw_ptr = right;
        return ty;
    }
    FuType *right = FuParser_parse_type(p, prec, FU_FALSE);
    FuSpan *sp = FuSpan_join(tok0.sp, right->sp);
    FuType *ty = FuType_new(p->pkg, sp, TY_PTR);
    ty->_ptr = right;
    return ty;
}

static FuType *FuParser_parse_array_slice_type(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuType *inner = FuParser_parse_type(p, 0, FU_TRUE);
    if (FuParser_check_token(p, TOK_SEMI)) {
        FuParser_bump(p);
        FuExpr *size = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
        FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
        FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
        FuType *ty = FuType_new(p->pkg, sp, TY_ARRAY);
        ty->_array.ty = inner;
        ty->_array.size = size;
        return ty;
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuType *ty = FuType_new(p->pkg, sp, TY_SLICE);
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
    ty = FuType_new(p->pkg, sp, TY_TUPLE);
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
    FATAL1(tok.sp, "invalid prefix type operator: `%s`", FuKind_ty_op_cstr(op));
    return NULL;
}

static FuType *FuParser_parse_suffix_type(FuParser *p, FuType *left, fu_ty_op_k op) {
    FuToken op_tok = FuParser_nth_token(p, 0);
    FuSpan *sp = FuSpan_join(op_tok.sp, left->sp);
    switch (op) {
    case TY_OP_NILABLE: {
        FuParser_bump(p);
        FuType *ty = FuType_new(p->pkg, sp, TY_NILABLE);
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
    FuSpan *old_left_sp;
    switch (op) {
    case TY_OP_TRANS: {
        FuVec *sig_tys = FuVec_new(sizeof(FuType *));
        while (1) {
            FuVec_push_ptr(sig_tys, left);
            old_left_sp = left->sp;
            if (!FuParser_check_token(p, TOK_RARROW)) {
                break;
            }
            FuParser_expect_token(p, TOK_RARROW);
            left = FuParser_parse_type(p, prec, FU_TRUE);
        }
        FuFnSig *sig = FuFnSig_new(NULL, sig_tys);
        FuSpan *sp = FuSpan_join(left->sp, old_left_sp);
        FuType *fn = FuType_new(p->pkg, sp, TY_FN_SIG);
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
    case TOK_LT: {
        FuPath *path;
        FuAnno *anno = FuParser_parse_anno(p, &path);
        FuParser_expect_token(p, TOK_MOD_SEP);
        FuPathItem *item = FuParser_parse_path_item(p);
        FuPath_push_item(path, item);
        prefix_type = FuType_new_path(p->pkg, anno, path);
        break;
    }
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
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_ref = FU_FALSE;
    if (FuParser_check_token(p, TOK_STAR)) {
        is_ref = FU_TRUE;
        FuParser_expect_token(p, TOK_STAR);
    }
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_AT);
    FuPat *bind = FuParser_parse_pat(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(lo, bind->sp);
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
    if (tok.kd == TOK_IDENT) {
        FuIdent *ident = FuParser_parse_ident(p);
        FuSpan *sp = FuSpan_join(start_tok.sp, ident->sp);
        FuPat *pat = FuPat_new(sp, PAT_FIELD);
        pat->_field = ident;
        return pat;
    }
    FuLit *lit = FuParser_parse_lit(p);
    if (lit->kd != LIT_INT) {
        FATAL1(tok.sp, "expect int or identifer, find: `%s`", FuToken_kind_csr(tok));
    }
    FuSpan *sp = FuSpan_join(start_tok.sp, lit->sp);
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
    FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE, NULL);
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
        FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE, NULL);
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
        FuExpr *expr = FuParser_parse_expr(p, FuOp_precedence(OP_BIT_OR), FU_TRUE, NULL);
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
        if (path->is_macro) {
            ERROR(path->sp, "macro is not allowed in pattern");
        }
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
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    if (!expr) {
        return args;
    }
    while (1) {
        FuVec_push_ptr(args, expr);
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd != TOK_COMMA) {
            break;
        }
        FuParser_bump(p);
        expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
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
    FuToken tok0 = FuParser_nth_token(p, 0);
    FuIdent *ident;
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

static FuExpr *FuParser_parse_suffix_macro_expr(FuParser *p, FuExpr *left) {
    FuPath *path = FuParser_parse_path(p);
    if (!path->is_macro) {
        ERROR(path->sp, "expect a macro path, but find a normal path");
    }
    if (!FuParser_check_token_fn(p, FuToken_is_open_delim)) {
        FuToken tok = FuParser_nth_token(p, 0);
        FATAL1(tok.sp, "expect `(`, `[`, `{`, find: `%s`", FuToken_kind_csr(tok));
    }
    FuVec *tokens = FuVec_new(sizeof(FuToken));
    FuParser_parse_tok_group(p, tokens);
    FuToken *last_tok = FuVec_last(tokens);
    FuSpan *sp = FuSpan_join(left->sp, last_tok->sp);
    FuMacroCall *call = FuMacroCall_new(sp, FU_TRUE, path);
    call->left = left;
    call->args = tokens;
    FuExpr *expr = FuExpr_new(sp, EXPR_MACRO_CALL);
    expr->_macro_call = call;
    return expr;
}

static FuExpr *FuParser_parse_dot_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_DOT);
    FuToken tok0 = FuParser_nth_token(p, 0);
    FuToken tok1 = FuParser_nth_token(p, 1);
    if (tok0.kd == TOK_MACRO || (FuToken_is_ident(tok0) && tok1.kd == TOK_MOD_SEP)) {
        return FuParser_parse_suffix_macro_expr(p, left);
    }
    if (tok0.kd == TOK_IDENT && (tok1.kd == TOK_OPEN_PAREN || tok1.kd == TOK_POUND)) {
        /* `base.method_call()`, `base.method_call#<>()` */
        return FuParser_parse_method_call_expr(p, left);
    }
    return FuParser_parse_field_expr(p, left);
}

static FuExpr *FuParser_parse_index_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuExpr *idx = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuToken tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(left->sp, tok.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_INDEX);
    expr->_index.base = left;
    expr->_index.idx = idx;
    return expr;
}

static FuFieldInit *FuParser_parse_field_init(FuParser *p) {
    FuToken tok0, tok1;
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    /* todo: parse attr */
    tok0 = FuParser_nth_token(p, 0);
    if (tok0.kd == TOK_DOT) {
        tok1 = FuParser_nth_token(p, 1);
        FuParser_bump(p);
        if (tok1.kd == TOK_IDENT) {
            FuIdent *ident = FuParser_parse_ident(p);
            FuParser_expect_token(p, TOK_EQ);
            FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
            FuSpan *sp = FuSpan_join(tok0.sp, expr->sp);
            FuFieldInit *init = FuFieldInit_new(sp, FLD_NAME, attrs);
            init->_name.ident = ident;
            init->_name.init = expr;
            return init;
        } else if (tok1.kd == TOK_INT) {
            FuLit *lit = FuParser_parse_lit(p);
            FuParser_expect_token(p, TOK_EQ);
            FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
            FuSpan *sp = FuSpan_join(tok0.sp, expr->sp);
            FuFieldInit *init = FuFieldInit_new(sp, FLD_INDEX, attrs);
            init->_index.lit = lit;
            init->_index.init = expr;
            return init;
        }
        FATAL1(tok1.sp, "expect identifier name or array index, find `%s`", FuToken_kind_csr(tok1));
    }
    if (tok0.kd == TOK_DOT_DOT_DOT) {
        FuParser_bump(p);
        FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
        FuSpan *sp = FuSpan_join(tok0.sp, expr->sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_BASE, attrs);
        init->_base = expr;
        return init;
    }
    if (tok0.kd == TOK_SEMI) {
        FuParser_bump(p);
        FuExpr *size = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
        FuSpan *sp = FuSpan_join(tok0.sp, size->sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_SIZE, attrs);
        init->_size = size;
        return init;
    }
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    tok1 = FuParser_nth_token(p, 0);
    if (tok1.kd == TOK_DOT_DOT_DOT) {
        FuParser_bump(p);
        FuSpan *sp = FuSpan_join(expr->sp, tok1.sp);
        FuFieldInit *init = FuFieldInit_new(sp, FLD_REPEAT, attrs);
        init->_repeat = expr;
        return init;
    }
    FuFieldInit *init = FuFieldInit_new(expr->sp, FLD_EXPR, attrs);
    init->_expr = expr;
    return init;
}

static FuVec *FuParser_parse_field_inits(FuParser *p) {
    FuVec *inits = FuVec_new(sizeof(FuNode *));
    while (1) {
        FuFieldInit *init = FuParser_parse_field_init(p);
        FuVec_push_ptr(inits, init);
        if (FuParser_check_token(p, TOK_COMMA)) {
            FuParser_expect_token(p, TOK_COMMA);
            continue;
        }
        break;
    }
    return inits;
}

static FuExpr *FuParser_parse_struct_expr(FuParser *p, FuExpr *left) {
    FuParser_expect_token(p, TOK_MOD_SEP);
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_OPEN_BRACE) {
        FuParser_expect_token(p, TOK_OPEN_BRACE);
        FuVec *inits = FuParser_parse_field_inits(p);
        FuToken tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
        FuSpan *sp = FuSpan_join(left->sp, tok.sp);
        FuExpr *expr = FuExpr_new(sp, EXPR_STRUCT);
        expr->_struct.base = left;
        expr->_struct.field_inits = inits;
        return expr;
    }
    if (tok.kd == TOK_OPEN_PAREN) {
        FuParser_expect_token(p, TOK_OPEN_PAREN);
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
    if (left) {
        start_sp = left->sp;
    } else {
        start_sp = FuParser_current_span(p);
    }
    FuToken op_tok = FuParser_bump(p);
    FuExpr *right = FuParser_parse_expr(p, prec, FU_FALSE, NULL);
    FuSpan *end_sp;
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
        FuExpr *right = FuParser_parse_expr(p, prec, FU_TRUE, NULL);
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
        FuExpr *right = FuParser_parse_expr(p, prec, FU_TRUE, NULL);
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
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuSpan *sp = FuSpan_join(tok.sp, expr->sp);
    FuExpr *cond = FuExpr_new(sp, EXPR_LET_COND);
    cond->_let_cond.pat = pat;
    cond->_let_cond.expr = expr;
    return cond;
}

static FuExpr *FuParser_parse_group_or_tuple_expr(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_PAREN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
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
        expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
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

static fu_bool_t FuParser_check_item_declare(FuParser *p, fu_keyword_k kd) {
    FuToken tok;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd != TOK_KEYWORD) {
            return FU_FALSE;
        }
        if (tok.sym == kd) {
            return FU_TRUE;
        }
        if (tok.sym == KW_PKG || tok.sym == KW_PUB) {
            i++;
            continue;
        }
        break;
    }
    return FU_FALSE;
};

static fu_bool_t FuParser_check_interface(FuParser *p) {
    FuToken tok;
    fu_size_t i = 0;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd != TOK_KEYWORD) {
            return FU_FALSE;
        }
        if (tok.sym == KW_INTERFACE) {
            return FU_TRUE;
        }
        if (tok.sym == KW_UNSAFE) {
            i++;
            continue;
        }
        break;
    }
    return FU_FALSE;
}

static fu_bool_t FuParser_check_item_macro_call(FuParser *p) {
    fu_size_t i = 0;
    FuToken tok;
    while (1) {
        tok = FuParser_nth_token(p, i);
        if (tok.kd != TOK_MACRO && FuToken_is_ident(tok)) {
            FuToken next_tok = FuParser_nth_token(p, i + 1);
            if (next_tok.kd == TOK_MOD_SEP) {
                i += 2;
                continue;
            }
        }
        if (tok.kd == TOK_MACRO) {
            return FU_TRUE;
        }
        break;
    }
    return FU_FALSE;
}

FuNode *FuParser_parse_block_item(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_outer_attrs(p, attrs);

    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_KEYWORD) {
        switch (tok.sym) {
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

    FuGeneric *ge = NULL;
    if (FuParser_check_2_token(p, TOK_POUND, TOK_LT)) {
        ge = FuParser_parse_ge(p);
    }
    if (FuParser_check_fn(p)) {
        /* must before const item */
        return FuParser_parse_item_fn(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_STRUCT)) {
        return FuParser_parse_item_struct(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_ENUM)) {
        return FuParser_parse_item_enum(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_UNION)) {
        return FuParser_parse_item_union(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_TYPE)) {
        return FuParser_parse_item_alias(p, attrs, ge);
    }
    if (FuParser_check_interface(p)) {
        return FuParser_parse_item_interface(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_EXTENSION)) {
        return FuParser_parse_item_extension(p, attrs, ge);
    }
    if (ge) {
        FuSpan *err_sp = FuParser_current_span(p);
        FATAL(err_sp,
              "generics must be followed by `fn`, `struct`, `enum`, `union`, `type`, `interface`, `extension` item");
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
    if (FuParser_check_item_declare(p, KW_USE)) {
        return FuParser_parse_item_use(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_STATIC)) {
        return FuParser_parse_item_static(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_CONST)) {
        return FuParser_parse_item_const(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_EXTERN)) {
        return FuParser_parse_item_extern(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_MACRO)) {
        return FuParser_parse_item_macro_def(p, attrs);
    }
    FuExpr *prefix_expr = NULL;
    if (FuParser_check_item_macro_call(p)) {
        FuMacroCall *call = FuParser_parse_prefix_macro_call(p);
        if (FuParser_check_newline(p)) {
            /* node macro must end with newline/eof */
            FuNode *nd = FuNode_new(p->pkg, call->sp, ND_MACRO_CALL);
            nd->attrs = attrs;
            nd->_macro_call = call;
            return nd;
        } else {
            /* otherwise it is a expr macro */
            prefix_expr = FuExpr_new(call->sp, EXPR_MACRO_CALL);
            prefix_expr->_macro_call = call;
        }
    }

    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, prefix_expr);
    if (!FuParser_check_token_fn(p, FuToken_is_assign)) {
        FuParser_expect_token(p, TOK_SEMI);
        FuNode *nd = FuNode_new(p->pkg, expr->sp, ND_EXPR);
        nd->attrs = attrs;
        nd->_expr.expr = expr;
        return nd;
    }
    return FuParser_parse_item_assign(p, attrs, expr);
}

FuBlock *FuParser_parse_block(FuParser *p) {
    FuToken open_tok = FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *items = FuVec_new(sizeof(FuNode *));
    while (1) {
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_CLOSE_BRACE) {
            break;
        }
        FuNode *item = FuParser_parse_block_item(p);
        FuVec_push_ptr(items, item);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(open_tok.sp, close_tok.sp);
    FuBlock *blk = FuBlock_new(sp);
    blk->items = items;
    return blk;
}

static FuFnParam *FuParser_parse_self_fn_param(FuParser *p) {
    FuToken tok0 = FuParser_nth_token(p, 0);
    if (FuToken_check_keyword(tok0, KW_SELF_LOWER)) {
        FuParser_bump(p);
        /* `self` */
        FuType *self_ty = FuType_from_keyword(p->pkg, tok0.sp, KW_SELF_UPPER);
        FuPat *self_pat = FuPat_new_path_from_tok(tok0);
        FuFnParam *param = FuFnParam_new(tok0.sp, self_pat);
        param->ty = self_ty;
        return param;
    }
    if (tok0.kd != TOK_STAR) {
        return NULL;
    }
    FuToken tok1 = FuParser_nth_token(p, 1);
    if (FuToken_check_keyword(tok1, KW_SELF_LOWER)) {
        FuParser_bump(p);
        FuParser_bump(p);
        /* `*self` */
        FuSpan *sp = FuSpan_join(tok0.sp, tok1.sp);

        FuType *self_ty = FuType_from_keyword(p->pkg, tok1.sp, KW_SELF_UPPER);
        FuType *ptr_self_ty = FuType_new(p->pkg, sp, TY_PTR);
        ptr_self_ty->_ptr = self_ty;

        FuPat *self_pat = FuPat_new_path_from_tok(tok1);
        FuFnParam *param = FuFnParam_new(sp, self_pat);
        param->ty = ptr_self_ty;
        return param;
    }
    if (!(tok1.kd == TOK_IDENT && tok1.sym == FuPkg_intern_cstr(p->pkg, "raw"))) {
        return NULL;
    }
    FuToken tok2 = FuParser_nth_token(p, 2);
    if (FuToken_check_keyword(tok0, KW_SELF_LOWER)) {
        FuParser_bump(p);
        FuParser_bump(p);
        FuParser_bump(p);
        /* `*raw self` */
        FuSpan *sp = FuSpan_join(tok0.sp, tok2.sp);

        FuType *self_ty = FuType_from_keyword(p->pkg, tok2.sp, KW_SELF_UPPER);
        FuType *raw_ptr_self_ty = FuType_new(p->pkg, sp, TY_RAW_PTR);
        raw_ptr_self_ty->_ptr = self_ty;

        FuPat *self_pat = FuPat_new_path_from_tok(tok2);
        FuFnParam *param = FuFnParam_new(sp, self_pat);
        param->ty = raw_ptr_self_ty;
        return param;
    }
    return NULL;
}

static FuFnParam *FuParser_parse_fn_param(FuParser *p, fu_bool_t check_ty) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_outer_attrs(p, attrs);
    /* can not use `|` pat because clousrue use `|...|` */
    FuPat *pat = FuParser_parse_pat(p, 1, FU_TRUE);
    FuType *ty;
    if (check_ty) {
        /* parse fn or method params */
        FuParser_expect_token(p, TOK_COLON);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        /* parse closure params */
        if (FuParser_check_token(p, TOK_COLON)) {
            FuParser_expect_token(p, TOK_COLON);
            ty = FuParser_parse_type(p, 0, FU_TRUE);
        } else {
            ty = FuType_from_keyword(p->pkg, pat->sp, KW_UNDERSCORE);
        }
    }
    FuFnParam *param = FuFnParam_new(pat->sp, pat);
    param->attrs = attrs;
    param->ty = ty;
    return param;
}

static FuVec *FuParser_parse_fn_params(FuParser *p, fu_token_k close_kd, fu_bool_t check_ty) {
    FuVec *params = FuVec_new(sizeof(FuFnParam));
    if (FuParser_check_token(p, close_kd)) {
        return params;
    }
    fu_bool_t is_first = FU_TRUE;
    while (1) {
        FuFnParam *param;
        if (is_first) {
            param = FuParser_parse_self_fn_param(p);
            if (!param) {
                param = FuParser_parse_fn_param(p, check_ty);
            }
            is_first = FU_FALSE;
        } else {
            param = FuParser_parse_fn_param(p, check_ty);
        }
        FuVec_push_ptr(params, param);
        if (FuParser_check_token(p, TOK_COMMA)) {
            FuParser_expect_token(p, TOK_COMMA);
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
    if (FuParser_check_token(p, TOK_OR_OR)) {
        /* `|| expr` */
        params = FuVec_new(sizeof(FuFnParam *));
        FuParser_expect_token(p, TOK_OR_OR);
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
        return_ty = FuType_from_keyword(p->pkg, NULL, KW_UNDERSCORE);
    }
    FuFnSig *sig = FuFnSig_from_params(p->pkg, NULL, params, return_ty);
    FuExpr *body = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuSpan *sp = FuSpan_join(lo, body->sp);

    FuType *ty = FuType_new_fn_sig(p->pkg, sp, sig);
    FuExpr *expr = FuExpr_new(sp, EXPR_CLOSURE);
    expr->ty = ty;
    expr->_closure.is_async = is_async;
    expr->_closure.is_unsafe = is_unsafe;
    expr->_closure.params = params;
    expr->_closure.body = body;
    return expr;
}

static FuExpr *FuParser_parse_await_expr(FuParser *p) {
    FuToken tok = FuParser_expect_keyword(p, KW_AWAIT);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuSpan *sp = FuSpan_join(tok.sp, expr->sp);
    FuExpr *await = FuExpr_new(sp, EXPR_AWAIT);
    await->_await.expr = expr;
    return await;
}

static FuExpr *FuParser_parse_if_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_IF);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuExpr *on_true = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuParser_expect_keyword(p, KW_ELSE);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuExpr *on_false = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuToken end = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end.sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_IF);
    expr->_if.cond = cond;
    expr->_if.on_true = on_true;
    expr->_if.on_false = on_false;
    return expr;
}

static FuExpr *FuParser_parse_block_expr(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_async = FuParser_eat_keyword(p, KW_ASYNC);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    FuBlock *blk = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, blk->sp);
    FuExpr *expr = FuExpr_new(sp, EXPR_BLOCK);
    expr->_block.is_async = is_async;
    expr->_block.is_unsafe = is_unsafe;
    expr->_block.block = blk;
    return expr;
}

static FuExpr *FuParser_parse_prefix_macro_expr(FuParser *p, FuPath *prefix) {
    assert(prefix->is_macro);
    if (!FuParser_check_token_fn(p, FuToken_is_open_delim)) {
        FuToken tok = FuParser_nth_token(p, 0);
        FATAL1(tok.sp, "expect `(`, `[`, `{`, find: `%s`", FuToken_kind_csr(tok));
    }
    FuVec *tokens = FuVec_new(sizeof(FuToken));
    FuParser_parse_tok_group(p, tokens);
    FuToken *last_tok = FuVec_last(tokens);
    FuSpan *sp = FuSpan_join(prefix->sp, last_tok->sp);
    FuMacroCall *call = FuMacroCall_new(sp, FU_FALSE, prefix);
    call->args = tokens;
    FuExpr *expr = FuExpr_new(sp, EXPR_MACRO_CALL);
    expr->_macro_call = call;
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
    if (FuToken_is_ident(tok)) {
        FuPath *path = FuParser_parse_path(p);
        if (path->is_macro) {
            return FuParser_parse_prefix_macro_expr(p, path);
        } else {
            return FuExpr_new_path(NULL, path);
        }
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
    if (FuParser_check_block(p)) {
        return FuParser_parse_block_expr(p);
    }
    if (FuParser_check_closure(p)) {
        return FuParser_parse_closure_expr(p);
    }
    FATAL1(tok.sp, "invalid expr: `%s`", FuKind_keyword_cstr(tok.sym));
    return NULL;
}

static FuExpr *FuParser_parse_left_expr(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null) {
    FuExpr *prefix_expr = NULL;
    FuToken tok = FuParser_nth_token(p, 0);
    switch (tok.kd) {
    case TOK_KEYWORD: {
        prefix_expr = FuParser_parse_keyword_expr(p);
        break;
    }
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
    case TOK_OPEN_PAREN:
        prefix_expr = FuParser_parse_group_or_tuple_expr(p);
        break;
    case TOK_OPEN_BRACKET:
        prefix_expr = FuParser_parse_array_expr(p);
        break;
    case TOK_OPEN_BRACE:
        prefix_expr = FuParser_parse_block_expr(p);
        break;
    case TOK_LT: {
        FuPath *path;
        FuAnno *anno = FuParser_parse_anno(p, &path);
        FuParser_expect_token(p, TOK_MOD_SEP);
        FuPathItem *item = FuParser_parse_path_item(p);
        FuPath_push_item(path, item);
        prefix_expr = FuExpr_new_path(anno, path);
        break;
    }
    case TOK_MACRO:
    case TOK_IDENT: {
        FuPath *path = FuParser_parse_path(p);
        if (path->is_macro) {
            prefix_expr = FuParser_parse_prefix_macro_expr(p, path);
        } else {
            prefix_expr = FuExpr_new_path(NULL, path);
        }
        break;
    }
    case TOK_OR:
    case TOK_OR_OR: {
        prefix_expr = FuParser_parse_closure_expr(p);
        break;
    }
    default: {
        fu_op_k prefix_op;
        if (FuToken_to_prefix_op(tok, &prefix_op)) {
            prefix_expr = FuParser_parse_prefix_expr(p, prefix_op, FuOp_precedence(prefix_op));
        }
        break;
    }
    }
    return prefix_expr;
}

FuExpr *FuParser_parse_expr(FuParser *p, fu_op_prec_t prec, fu_bool_t check_null, FuExpr *prefix_expr) {
    FuToken tok = FuParser_nth_token(p, 0);
    FuExpr *left = prefix_expr ? prefix_expr : FuParser_parse_left_expr(p, prec, check_null);
    if (check_null && !left) {
        FATAL1(tok.sp, "expect expression, find `%s`", FuToken_kind_csr(tok));
    }
    /* parse infix ops */
    while (1) {
        FuToken tok = FuParser_nth_token(p, 0);

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

void FuParser_parse_tok_group(FuParser *p, FuVec *tokens) {
    fu_bool_t old_tok_level = p->tok_level;
    p->tok_level = TOK_LEVEL_NO_BLANK;
    FuToken tok = FuParser_nth_token(p, 0);
    if (FuToken_is_close_delim(tok)) {
        FATAL1(tok.sp, "unblance tok tree group delimiter: `%s`", FuToken_kind_csr(tok));
    }
    if (!FuToken_is_open_delim(tok)) {
        FuParser_bump(p);
        FuVec_push(tokens, &tok);
        return;
    }
    FuToken open_tok = FuParser_expect_token_fn(p, FuToken_is_open_delim, "tok tree group open delimiter");
    FuVec_push(tokens, &open_tok);
    while (1) {
        tok = FuParser_nth_token(p, 0);
        if (FuToken_is_match_delim(open_tok, tok)) {
            break;
        }
        if (FuToken_is_close_delim(tok)) {
            FATAL1(tok.sp, "unblance tok tree group delimiter: `%s`", FuToken_kind_csr(tok));
        }
        FuParser_parse_tok_group(p, tokens);
    }
    FuToken close_tok = FuParser_expect_token_fn(p, FuToken_is_close_delim, "tok tree group close delimiter");
    FuVec_push(tokens, &close_tok);
    p->tok_level = old_tok_level;
}

FuAttr *FuParser_parse_normal_attr(FuParser *p) {
    fu_bool_t is_outer;
    FuToken start_tok;
    if (FuParser_check_2_token(p, TOK_POUND, TOK_NOT)) {
        is_outer = FU_FALSE;
        start_tok = FuParser_expect_token(p, TOK_POUND);
        FuParser_expect_token(p, TOK_NOT);
    } else if (FuParser_check_2_token(p, TOK_POUND, TOK_OPEN_BRACKET)) {
        is_outer = FU_TRUE;
        start_tok = FuParser_expect_token(p, TOK_POUND);
    } else {
        FuToken err_tok = FuParser_nth_token(p, 0);
        FATAL1(err_tok.sp, "expect attr, find: `%s`", FuToken_kind_csr(err_tok));
    }
    FuParser_expect_token(p, TOK_OPEN_BRACKET);
    FuPath *path = FuParser_parse_path(p);
    if (path->is_macro) {
        ERROR(path->sp, "macro is not allowed in attribute path");
    }
    FuVec *tokens = FuVec_new(sizeof(FuToken));
    if (FuParser_check_token_fn(p, FuToken_is_open_delim)) {
        FuParser_parse_tok_group(p, tokens);
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACKET);
    FuSpan *sp = FuSpan_join(start_tok.sp, end_tok.sp);
    FuAttr *attr = FuAttr_new(sp, ATTR_NORMAL, is_outer);
    attr->_normal.path = path;
    attr->_normal.args = tokens;
    return attr;
}

FuAttr *FuParser_parse_doc_attr(FuParser *p) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_DOC_COMMENT) {
        FATAL1(tok.sp, "expect attr, find: `%s`", FuToken_kind_csr(tok));
    }
    FuParser_expect_token(p, TOK_DOC_COMMENT);
    fu_bool_t is_outer = FuToken_is_outer_doc_comment(tok);
    FuAttr *attr = FuAttr_new(tok.sp, ATTR_DOC, is_outer);
    FuStr *symbol = FuPkg_get_symbol(p->pkg, tok.sym);
    attr->_doc = FuStr_from_slice(symbol, 1, FuStr_len(symbol));
    return attr;
}

void FuParser_parse_inner_attrs(FuParser *p, FuVec *attrs) {
    while (1) {
        /* `#![attr]`, `#![attr(...)] */
        if (FuParser_check_2_token(p, TOK_POUND, TOK_NOT)) {
            FuAttr *attr = FuParser_parse_normal_attr(p);
            FuVec_push_ptr(attrs, attr);
            continue;
        }
        /* `//! ...` */
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_DOC_COMMENT && !FuToken_is_outer_doc_comment(tok)) {
            FuAttr *attr = FuParser_parse_doc_attr(p);
            FuVec_push_ptr(attrs, attr);
            continue;
        }
        break;
    }
}

void FuParser_parse_outer_attrs(FuParser *p, FuVec *attrs) {
    while (1) {
        /* `#[attr]`, `#[attr(...)] */
        if (FuParser_check_2_token(p, TOK_POUND, TOK_OPEN_BRACKET)) {
            FuAttr *attr = FuParser_parse_normal_attr(p);
            FuVec_push_ptr(attrs, attr);
            continue;
        }
        /* `/// ...` */
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_DOC_COMMENT && FuToken_is_outer_doc_comment(tok)) {
            FuAttr *attr = FuParser_parse_doc_attr(p);
            FuVec_push_ptr(attrs, attr);
            continue;
        }
        break;
    }
}

fu_vis_k FuParser_parse_visibility(FuParser *p, fu_vis_k def) {
    FuToken tok = FuParser_nth_token(p, 0);
    if (tok.kd != TOK_KEYWORD) {
        return def;
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
        return def;
        break;
    }
}

static FuUse *FuParser_parse_use_tree(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuSpan *last_sp = lo;
    FuToken tok = FuParser_nth_token(p, 0);
    FuPath *prefix = NULL;
    if (FuToken_is_ident(tok)) {
        prefix = FuParser_parse_path(p);
        last_sp = prefix->sp;
    }
    tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_SEMI || tok.kd == TOK_COMMA || FuParser_check_keyword(p, KW_AS)) {
        if (!prefix) {
            FATAL(tok.sp, "prefix can not be null");
        }
        FuIdent *alias = NULL;
        if (FuParser_check_keyword(p, KW_AS)) {
            FuParser_expect_keyword(p, KW_AS);
            alias = FuParser_parse_ident(p);
            last_sp = alias->sp;
            if (!((prefix->is_macro && alias->is_macro) || (!prefix->is_macro && !alias->is_macro))) {
                ERROR(alias->sp, "alias name does not match prefix");
            }
        }
        FuSpan *sp = FuSpan_join(lo, last_sp);
        if (prefix->is_macro) {
            FuUse *use = FuUse_new(sp, USE_MACRO, prefix);
            use->_macro.alias = alias;
            return use;
        }
        FuUse *use = FuUse_new(sp, USE_SIMPLE, prefix);
        use->_simple.alias = alias;
        return use;
    }
    if (prefix) {
        FuParser_expect_token(p, TOK_MOD_SEP);
    }
    tok = FuParser_nth_token(p, 0);
    if (tok.kd == TOK_STAR) {
        last_sp = tok.sp;
        FuParser_expect_token(p, TOK_STAR);
        tok = FuParser_nth_token(p, 0);
        if (tok.kd == TOK_NOT) {
            FuSpan *sp = FuSpan_join(lo, tok.sp);
            FuUse *use = FuUse_new(sp, USE_GLOB_MACRO, prefix);
            return use;
        }
        FuSpan *sp = FuSpan_join(lo, last_sp);
        FuUse *use = FuUse_new(sp, USE_GLOB, prefix);
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
    FuSpan *sp = FuSpan_join(lo, tok.sp);
    FuUse *use = FuUse_new(sp, USE_NESTED, prefix);
    use->_nested = nested;
    return use;
}

FuNode *FuParser_parse_item_use(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_USE);
    FuUse *tree = FuParser_parse_use_tree(p);
    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_USE);
    nd->attrs = attrs;
    nd->_use.vis = vis;
    nd->_use.tree = tree;
    return nd;
}

FuNode *FuParser_parse_item_static(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_STATIC);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuExpr *expr = NULL;
    if (!FuParser_check_token(p, TOK_SEMI)) {
        FuParser_expect_token(p, TOK_EQ);
        expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    }
    FuToken end = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_STATIC);
    nd->attrs = attrs;
    nd->_static.vis = vis;
    nd->_static.ident = ident;
    nd->_static.ty = ty;
    nd->_static.init = expr;
    return nd;
}

FuNode *FuParser_parse_item_const(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_CONST);
    FuIdent *ident = FuParser_parse_ident(p);
    FuType *ty;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_bump(p);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        ty = FuType_from_keyword(p->pkg, ident->sp, KW_UNDERSCORE);
    }
    FuParser_expect_token(p, TOK_EQ);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, expr->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_CONST);
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
        expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_LET);
    nd->attrs = attrs;
    nd->_let.pat = pat;
    nd->_let.ty = ty;
    nd->_let.init = expr;
    return nd;
}

FuNode *FuParser_parse_item_assign(FuParser *p, FuVec *attrs, FuExpr *lexpr) {
    FuToken op_tok = FuParser_expect_token_fn(p, FuToken_is_assign, "assign operater");
    FuExpr *rexpr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lexpr->sp, rexpr->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_ASSIGN);
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
    FuLabel *label = FuParser_parse_label(p);
    FuSpan *sp;
    if (label) {
        sp = FuSpan_join(tok.sp, label->sp);
    } else {
        sp = tok.sp;
    }
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_BREAK);
    nd->attrs = attrs;
    nd->_break.label = label;
    nd->_break.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_continue(FuParser *p, FuVec *attrs) {
    FuToken tok = FuParser_expect_keyword(p, KW_CONTINUE);
    FuLabel *label = FuParser_parse_label(p);
    FuSpan *sp;
    if (label) {
        sp = FuSpan_join(tok.sp, label->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_CONTINUE);
    nd->attrs = attrs;
    nd->_continue.label = label;
    return nd;
}

FuNode *FuParser_parse_item_yield(FuParser *p, FuVec *attrs) {
    FuToken tok = FuParser_expect_keyword(p, KW_YIELD);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    FuSpan *sp;
    if (expr) {
        sp = FuSpan_join(tok.sp, expr->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_YIELD);
    nd->attrs = attrs;
    nd->_yield.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_throw(FuParser *p, FuVec *attrs) {
    FuToken tok = FuParser_expect_keyword(p, KW_THROW);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    FuSpan *sp;
    if (expr) {
        sp = FuSpan_join(tok.sp, FuParser_current_span(p));
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_THROW);
    nd->attrs = attrs;
    nd->_throw.expr = expr;
    return nd;
}

FuNode *FuParser_parse_item_return(FuParser *p, FuVec *attrs) {
    FuToken tok = FuParser_expect_keyword(p, KW_RETURN);
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_FALSE, NULL);
    FuSpan *sp;
    if (expr) {
        sp = FuSpan_join(tok.sp, expr->sp);
    } else {
        sp = tok.sp;
    }
    FuParser_expect_token(p, TOK_SEMI);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_RETURN);
    nd->attrs = attrs;
    nd->_return.expr = expr;
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
    FuNode *nd = FuNode_new(p->pkg, sp, ND_IF);
    nd->_if.cond = NULL;
    nd->_if.block = block;
    nd->_if.next_if = NULL;
    return nd;
}

FuNode *FuParser_parse_item_if(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_IF);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_IF);
    nd->attrs = attrs;
    nd->_if.cond = cond;
    nd->_if.block = block;
    nd->_if.next_if = FuParser_parse_next_if_node(p);
    return nd;
}

FuNode *FuParser_parse_item_match(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_MATCH);
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuVec *arms = FuVec_new(sizeof(FuArm *));
    while (1) {
        FuVec *attrs = FuVec_new(sizeof(FuAttr *));
        /* todo: parse attrs */
        FuPat *pat = FuParser_parse_pat(p, 0, FU_TRUE);
        FuExpr *guard = NULL;
        if (FuParser_check_keyword(p, KW_IF)) {
            FuParser_expect_keyword(p, KW_IF);
            guard = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
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
    FuNode *nd = FuNode_new(p->pkg, sp, ND_MATCH);
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
    FuNode *nd = FuNode_new(p->pkg, sp, ND_LOOP);
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
    FuExpr *cond = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_WHILE);
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
    FuExpr *expr = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
    FuBlock *block = FuParser_parse_block(p);
    FuSpan *sp = FuSpan_join(lo, block->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_FOR);
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
            arm_guard = FuParser_parse_expr(p, 0, FU_TRUE, NULL);
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
    FuNode *nd = FuNode_new(p->pkg, sp, ND_TRY);
    nd->attrs = attrs;
    nd->_try.block = block;
    nd->_try.arms = arms;
    nd->_try.finally = finally;
    return nd;
}

static void FuParser_parse_fn(FuParser *p, FuVec *attrs, FuGeneric *ge, FuSpan **sp_p, FuIdent **ident_p,
                              FuVec **params_p, FuFnSig **sig_p, FuBlock **body_p) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_const = FuParser_eat_keyword(p, KW_CONST);
    fu_bool_t is_async = FuParser_eat_keyword(p, KW_ASYNC);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    if (is_const && is_async) {
        ERROR(lo, "fn can not be `const` and `async` at the same time");
    }
    FuParser_expect_keyword(p, KW_FN);
    *ident_p = FuParser_parse_ident(p);

    FuParser_expect_token(p, TOK_OPEN_PAREN);
    *params_p = FuParser_parse_fn_params(p, TOK_CLOSE_PAREN, FU_TRUE);
    FuParser_expect_token(p, TOK_CLOSE_PAREN);

    FuType *return_ty;
    if (FuParser_check_token(p, TOK_RARROW)) {
        FuParser_expect_token(p, TOK_RARROW);
        return_ty = FuParser_parse_type(p, 0, FU_TRUE);
    } else {
        return_ty = FuType_from_keyword(p->pkg, NULL, KW_NIL);
    }
    FuFnSig *sig = FuFnSig_from_params(p->pkg, NULL, *params_p, return_ty);
    sig->ge = ge;
    sig->is_const = is_const;
    sig->is_async = is_async;
    sig->is_unsafe = is_unsafe;
    *sig_p = sig;
    FuSpan *hi;
    *body_p = NULL;
    if (FuParser_check_token(p, TOK_SEMI)) {
        FuToken tok = FuParser_expect_token(p, TOK_SEMI);
        hi = tok.sp;
    } else if (FuParser_check_token(p, TOK_OPEN_BRACE)) {
        *body_p = FuParser_parse_block(p);
        hi = (*body_p)->sp;
    } else {
        FuToken tok = FuParser_nth_token(p, 1);
        FATAL1(tok.sp, "expect `{` or `;`, find: `%s`", FuToken_kind_csr(tok));
    }
    *sp_p = FuSpan_join(lo, hi);
}

FuNode *FuParser_parse_item_fn(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *sp;
    FuIdent *ident;
    FuVec *params;
    FuBlock *body;
    FuFnSig *sig;
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_parse_fn(p, attrs, ge, &sp, &ident, &params, &sig, &body);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_FN);
    nd->attrs = attrs;
    nd->_fn.ge = ge;
    nd->_fn.vis = vis;
    nd->_fn.ident = ident;
    nd->_fn.params = params;
    nd->_fn.sig = sig;
    nd->_fn.body = body;
    return nd;
}

static FuFieldDef *FuParser_parse_field_def(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuFieldDef *));
    FuParser_parse_outer_attrs(p, attrs);
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuSpan *sp = FuSpan_join(lo, ty->sp);
    FuFieldDef *def = FuFieldDef_new(sp, attrs, vis);
    def->ident = ident;
    def->ty = ty;
    return def;
}

static FuVariant *FuParser_parse_struct_variant(FuParser *p, FuVec *attrs, fu_vis_k vis, FuIdent *ident) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *fields = FuVec_new(sizeof(FuFieldDef *));
    while (1) {
        FuFieldDef *def = FuParser_parse_field_def(p);
        FuVec_push_ptr(fields, def);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuVariant *va = FuVariant_new(sp, attrs, vis, VA_STRUCT);
    va->ident = ident;
    va->_struct.fields = fields;
    return va;
}

static FuVariant *FuParser_parse_tuple_struct_variant(FuParser *p, FuVec *attrs, fu_vis_k vis, FuIdent *ident) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_token(p, TOK_OPEN_PAREN);
    fu_size_t i = 0;
    FuVec *fields = FuVec_new(sizeof(FuFieldDef *));
    while (1) {
        FuVec *def_attrs = FuVec_new(sizeof(FuVec *));
        FuParser_parse_outer_attrs(p, def_attrs);
        FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
        FuIdent *idx = FuIdent_from_idx(p->pkg, ty->sp, i);
        FuFieldDef *def = FuFieldDef_from_idx_type(def_attrs, idx, ty);
        FuVec_push_ptr(fields, def);
        if (FuParser_check_token(p, TOK_CLOSE_PAREN)) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
        i++;
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_PAREN);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuVariant *va = FuVariant_new(sp, attrs, vis, VA_TUPLE);
    va->ident = ident;
    va->_tuple.fields = fields;
    return va;
}

static FuVariant *FuParser_parse_unit_variant(FuParser *p, FuVec *attrs, fu_vis_k vis, FuIdent *ident) {
    FuLit *init = NULL;
    if (FuParser_check_token(p, TOK_EQ)) {
        init = FuParser_parse_lit(p);
    }
    FuVariant *va = FuVariant_new(ident->sp, attrs, vis, VA_UNIT);
    va->ident = ident;
    va->_unit.init = init;
    return va;
}

FuNode *FuParser_parse_item_struct(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_STRUCT);
    FuIdent *ident = FuParser_parse_ident(p);
    FuToken tok = FuParser_nth_token(p, 0);
    FuVariant *va;
    if (tok.kd == TOK_OPEN_BRACE) {
        va = FuParser_parse_struct_variant(p, attrs, vis, ident);
    } else if (tok.kd == TOK_OPEN_PAREN) {
        va = FuParser_parse_tuple_struct_variant(p, attrs, vis, ident);
        FuParser_expect_token(p, TOK_SEMI);
    } else {
        va = FuVariant_new(ident->sp, attrs, vis, VA_UNIT);
        va->ident = ident;
        FuParser_expect_token(p, TOK_SEMI);
    }
    FuSpan *sp = FuSpan_join(lo, va->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_STRUCT);
    nd->_struct.ge = ge;
    nd->_struct.va = va;
    return nd;
}

FuNode *FuParser_parse_item_enum(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_ENUM);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *items = FuVec_new(sizeof(FuVariant *));
    while (1) {
        FuVec *va_attrs = FuVec_new(sizeof(FuAttr *));
        FuParser_parse_outer_attrs(p, va_attrs);
        fu_vis_k va_vis = FuParser_parse_visibility(p, VIS_INHERIT);
        FuIdent *va_ident = FuParser_parse_ident(p);
        FuVariant *va;
        if (FuParser_check_token(p, TOK_OPEN_BRACE)) {
            va = FuParser_parse_struct_variant(p, va_attrs, va_vis, va_ident);
        } else if (FuParser_check_token(p, TOK_OPEN_PAREN)) {
            va = FuParser_parse_tuple_struct_variant(p, va_attrs, va_vis, va_ident);
        } else {
            va = FuParser_parse_unit_variant(p, va_attrs, va_vis, va_ident);
        }
        FuVec_push_ptr(items, va);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
        FuParser_expect_token(p, TOK_COMMA);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_ENUM);
    nd->attrs = attrs;
    nd->_enum.ge = ge;
    nd->_enum.vis = vis;
    nd->_enum.ge = NULL;
    nd->_enum.ident = ident;
    nd->_enum.items = items;
    return nd;
}

FuNode *FuParser_parse_item_union(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_UNION);
    FuIdent *ident = FuParser_parse_ident(p);
    FuVariant *va = FuParser_parse_struct_variant(p, attrs, vis, ident);
    FuSpan *sp = FuSpan_join(lo, va->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_UNION);
    nd->_union.ge = ge;
    nd->_union.va = va;
    return nd;
}

static FuAssoc *FuParser_parse_const_assoc(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_CONST);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_COLON);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuLit *def = NULL;
    if (FuParser_check_token(p, TOK_EQ)) {
        FuParser_expect_token(p, TOK_EQ);
        def = FuParser_parse_lit(p);
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuAssoc *assoc = FuAssoc_new(sp, ASSOC_CONST);
    assoc->attrs = attrs;
    assoc->vis = vis;
    assoc->ident = ident;
    assoc->_const.ty = ty;
    assoc->_const.def = def;
    return assoc;
}

static FuAssoc *FuParser_parse_ty_alias_assoc(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_TYPE);
    FuIdent *ident = FuParser_parse_ident(p);

    FuVec *bounds;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_expect_token(p, TOK_COLON);
        bounds = FuParser_parse_type_param_bounds(p);
    } else {
        bounds = FuVec_new(sizeof(FuType *));
    }

    FuType *ty = NULL;
    if (FuParser_check_token(p, TOK_EQ)) {
        FuParser_expect_token(p, TOK_EQ);
        ty = FuParser_parse_type(p, 0, FU_TRUE);
    }

    FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuAssoc *assoc = FuAssoc_new(sp, ASSOC_TY_ALIAS);
    assoc->attrs = attrs;
    assoc->vis = vis;
    assoc->ident = ident;
    assoc->_ty_alias.bounds = bounds;
    assoc->_ty_alias.ty = ty;
    return assoc;
}

static FuAssoc *FuParser_parse_fn_assoc(FuParser *p, FuVec *attrs, fu_vis_k vis) {
    FuSpan *sp;
    FuIdent *ident;
    FuVec *params;
    FuBlock *body;
    FuFnSig *sig;
    FuParser_parse_fn(p, attrs, NULL, &sp, &ident, &params, &sig, &body);
    FuAssoc *assoc = FuAssoc_new(sp, ASSOC_FN);
    assoc->attrs = attrs;
    assoc->vis = vis;
    assoc->ident = ident;
    assoc->_fn.params = params;
    assoc->_fn.sig = sig;
    assoc->_fn.body = body;
    return assoc;
}

FuAssoc *FuParser_parse_assoc(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_outer_attrs(p, attrs);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuAssoc *assoc;
    if (FuParser_check_keyword(p, KW_CONST)) {
        assoc = FuParser_parse_const_assoc(p, attrs, vis);
    } else if (FuParser_check_keyword(p, KW_TYPE)) {
        assoc = FuParser_parse_ty_alias_assoc(p, attrs, vis);
    } else if (FuParser_check_fn(p)) {
        assoc = FuParser_parse_fn_assoc(p, attrs, vis);
    } else {
        FuToken tok = FuParser_nth_token(p, 0);
        FATAL1(tok.sp, "invalid assoc item: `%s`", FuToken_kind_csr(tok));
    }
    return assoc;
}

FuNode *FuParser_parse_item_interface(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    FuParser_expect_keyword(p, KW_INTERFACE);
    FuIdent *ident = FuParser_parse_ident(p);
    FuVec *supers;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_expect_token(p, TOK_COLON);
        supers = FuParser_parse_type_param_bounds(p);
    } else {
        supers = FuVec_new(sizeof(FuType *));
    }
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *assocs = FuVec_new(sizeof(FuAssoc *));
    while (1) {
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
        FuAssoc *item = FuParser_parse_assoc(p);
        FuVec_push_ptr(assocs, item);
    }
    FuToken close_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, close_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_INTERFACE);
    nd->attrs = attrs;
    nd->_interface.ge = ge;
    nd->_interface.vis = vis;
    nd->_interface.is_unsafe = is_unsafe;
    nd->_interface.ident = ident;
    nd->_interface.supers = supers;
    nd->_interface.assocs = assocs;
    return nd;
}

FuNode *FuParser_parse_item_alias(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_TYPE);
    FuIdent *ident = FuParser_parse_ident(p);
    FuParser_expect_token(p, TOK_EQ);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuParser_expect_token(p, TOK_SEMI);
    FuSpan *sp = FuSpan_join(lo, ty->sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_TY_ALIAS);
    nd->attrs = attrs;
    nd->_ty_alias.ge = ge;
    nd->_ty_alias.vis = vis;
    nd->_ty_alias.ident = ident;
    nd->_ty_alias.ty = ty;
    return nd;
}

FuNode *FuParser_parse_extern_item(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_outer_attrs(p, attrs);
    if (FuParser_check_keyword(p, KW_STATIC)) {
        return FuParser_parse_item_static(p, attrs);
    }
    if (FuParser_check_fn(p)) {
        return FuParser_parse_item_fn(p, attrs, NULL);
    }
    if (FuParser_check_keyword(p, KW_TYPE)) {
        return FuParser_parse_item_alias(p, attrs, NULL);
    }
    FuToken tok = FuParser_nth_token(p, 0);
    FATAL1(tok.sp, "expect extern item, find: `%s`", FuToken_kind_csr(tok));
    return NULL;
}

FuNode *FuParser_parse_item_extern(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    FuParser_expect_keyword(p, KW_EXTERN);
    FuLit *abi = NULL;
    if (!FuParser_check_token(p, TOK_OPEN_BRACE)) {
        abi = FuParser_parse_lit(p);
    }
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *declares = FuVec_new(sizeof(FuNode *));
    while (1) {
        FuNode *item = FuParser_parse_extern_item(p);
        FuVec_push_ptr(declares, item);
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_EXTERN);
    nd->attrs = attrs;
    nd->_extern.abi = abi;
    nd->_extern.declares = declares;
    return nd;
}

FuNode *FuParser_parse_item_extension(FuParser *p, FuVec *attrs, FuGeneric *ge) {
    FuSpan *lo = FuParser_current_span(p);
    fu_bool_t is_unsafe = FuParser_eat_keyword(p, KW_UNSAFE);
    FuParser_expect_keyword(p, KW_EXTENSION);
    FuType *ty = FuParser_parse_type(p, 0, FU_TRUE);
    FuType *interface = NULL;
    if (FuParser_check_token(p, TOK_COLON)) {
        FuParser_expect_token(p, TOK_COLON);
        interface = FuParser_parse_type(p, 0, FU_TRUE);
    }
    FuParser_expect_token(p, TOK_OPEN_BRACE);
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *assocs = FuVec_new(sizeof(FuAssoc *));
    while (1) {
        if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
            break;
        }
        FuAssoc *item = FuParser_parse_assoc(p);
        FuVec_push_ptr(assocs, item);
    }
    FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
    FuSpan *sp = FuSpan_join(lo, end_tok.sp);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_EXTENSION);
    nd->attrs = attrs;
    nd->_extension.ge = ge;
    nd->_extension.is_unsafe = is_unsafe;
    nd->_extension.ty = ty;
    nd->_extension.interface = interface;
    nd->_extension.assocs = assocs;
    return nd;
}

FuNode *FuParser_parse_mod_item(FuParser *p) {
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_outer_attrs(p, attrs);

    FuGeneric *ge = NULL;
    if (FuParser_check_2_token(p, TOK_POUND, TOK_LT)) {
        ge = FuParser_parse_ge(p);
    }
    if (FuParser_check_fn(p)) {
        /* must before const item */
        return FuParser_parse_item_fn(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_STRUCT)) {
        return FuParser_parse_item_struct(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_ENUM)) {
        return FuParser_parse_item_enum(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_UNION)) {
        return FuParser_parse_item_union(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_TYPE)) {
        return FuParser_parse_item_alias(p, attrs, ge);
    }
    if (FuParser_check_interface(p)) {
        return FuParser_parse_item_interface(p, attrs, ge);
    }
    if (FuParser_check_item_declare(p, KW_EXTENSION)) {
        return FuParser_parse_item_extension(p, attrs, ge);
    }
    if (ge) {
        FuSpan *err_sp = FuParser_current_span(p);
        FATAL(err_sp,
              "generics must be followed by `fn`, `struct`, `enum`, `union`, `type`, `interface`, `extension` item");
    }

    if (FuParser_check_item_declare(p, KW_USE)) {
        return FuParser_parse_item_use(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_STATIC)) {
        return FuParser_parse_item_static(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_CONST)) {
        return FuParser_parse_item_const(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_EXTERN)) {
        return FuParser_parse_item_extern(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_MOD)) {
        return FuParser_parse_item_mod(p, attrs);
    }
    if (FuParser_check_item_declare(p, KW_MACRO)) {
        return FuParser_parse_item_macro_def(p, attrs);
    }

    if (FuParser_check_item_macro_call(p)) {
        FuMacroCall *call = FuParser_parse_prefix_macro_call(p);
        FuNode *nd = FuNode_new(p->pkg, call->sp, ND_MACRO_CALL);
        nd->_macro_call = call;
        nd->attrs = attrs;
        return nd;
    }
    FuToken tok = FuParser_nth_token(p, 0);
    FATAL1(tok.sp, "expect item prefix keyword, find `%s`", FuToken_kind_csr(tok));
    return NULL;
}

FuVec *FuParser_parse_mod_items(FuParser *p, FuVec *attrs, fu_token_k end) {
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *items = FuVec_new(sizeof(FuNode *));
    while (1) {
        FuToken tok = FuParser_nth_token(p, 0);
        if (tok.kd == end) {
            break;
        }
        FuNode *item = FuParser_parse_mod_item(p);
        FuVec_push_ptr(items, item);
    }
    return items;
}

FuNode *FuParser_parse_item_mod(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_MOD);
    FuIdent *ident = FuParser_parse_ident(p);
    if (FuParser_check_token(p, TOK_OPEN_BRACE)) {
        FuParser_expect_token(p, TOK_OPEN_BRACE);
        FuVec *items = FuParser_parse_mod_items(p, attrs, TOK_CLOSE_BRACE);
        FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
        FuSpan *sp = FuSpan_join(lo, end_tok.sp);
        FuNode *nd = FuNode_new(p->pkg, sp, ND_MOD);
        nd->attrs = attrs;
        nd->_mod.vis = vis;
        nd->_mod.ident = ident;
        nd->_mod.is_inline = FU_TRUE;
        nd->_mod.inner_sp = sp;
        nd->_mod.items = items;
        return nd;
    } else if (FuParser_check_token(p, TOK_SEMI)) {
        FuToken end_tok = FuParser_expect_token(p, TOK_SEMI);
        FuStr *mod_name = FuPkg_get_symbol(p->pkg, ident->name);
        FuStr *mod_dir = FuStr_clone(p->cur_dir);
        FuStr_path_join(mod_dir, FuStr_clone(mod_name));
        FuStr *mod_file = FuStr_clone(mod_dir);
        FuStr_push_utf8_cstr(mod_file, ".fu");

        /* init parser state */
        FuParser_push_state(p);
        p->cur_dir = mod_dir;
        FuParser_for_file(p, mod_file);

        FuVec *items = FuParser_parse_mod_items(p, attrs, TOK_EOF);

        /* restore parser state */
        FuStr_drop(p->cur_dir);
        FuParser_unfor_file(p);
        FuParser_pop_state(p);

        FuSpan *inner_sp;
        if (FuVec_len(items) == 0) {
            FuStr *fconent = FuPkg_get_file(p->lexer->pkg, p->lexer->_file.fpath);
            fu_size_t len = FuStr_len(fconent);
            inner_sp = FuSpan_new(p->lexer->pkg, p->lexer->_file.fpath, 0, len, 1, 1);
        } else {
            FuNode *first = FuVec_first_ptr(items);
            FuNode *last = FuVec_last_ptr(items);
            inner_sp = FuSpan_join(first->sp, last->sp);
        }
        FuSpan *sp = FuSpan_join(lo, end_tok.sp);
        FuNode *nd = FuNode_new(p->pkg, sp, ND_MOD);
        nd->attrs = attrs;
        nd->_mod.vis = vis;
        nd->_mod.ident = ident;
        nd->_mod.is_inline = FU_FALSE;
        nd->_mod.inner_sp = inner_sp;
        nd->_mod.items = items;
        return nd;
    } else {
        FuToken tok = FuParser_nth_token(p, 0);
        FATAL1(tok.sp, "expeck `{`, `;`, find: `%s`", FuToken_kind_csr(tok));
    }
    /* can not be here */
    return NULL;
}

static void FuParser_parse_macro_pattern_template(FuParser *p, FuVec *patterns, FuVec *templates) {
    FuVec *pattern = FuVec_new(sizeof(FuToken));
    FuParser_parse_tok_group(p, pattern);
    if (FuVec_len(pattern) < 2) {
        FuToken *err_tok = FuVec_first(pattern);
        FATAL(err_tok->sp, "macro pattern must starts with `(`, `[`, `{`");
    }
    FuParser_expect_token(p, TOK_FAT_ARROW);
    FuVec *template = FuVec_new(sizeof(FuToken));
    FuParser_parse_tok_group(p, template);
    FuToken *template_first_tok = FuVec_first(template);
    if (template_first_tok->kd != TOK_OPEN_BRACE) {
        FATAL(template_first_tok->sp, "macro template must starts with `{`");
    }
    FuVec_push_ptr(patterns, pattern);
    FuVec_push_ptr(templates, template);
}

FuNode *FuParser_parse_item_macro_def(FuParser *p, FuVec *attrs) {
    FuSpan *lo = FuParser_current_span(p);
    fu_vis_k vis = FuParser_parse_visibility(p, VIS_PRIV);
    FuParser_expect_keyword(p, KW_MACRO);
    FuIdent *ident = FuParser_parse_ident(p);
    if (!ident->is_macro) {
        FATAL(ident->sp, "macro ident must ends with `!`");
    }
    FuVec *patterns = FuVec_new(sizeof(FuVec *));
    FuVec *templates = FuVec_new(sizeof(FuVec *));
    FuSpan *hi;
    if (FuParser_check_token(p, TOK_FAT_ARROW)) {
        FuParser_expect_token(p, TOK_FAT_ARROW);
        FuParser_expect_token(p, TOK_OPEN_BRACE);
        while (1) {
            FuParser_parse_macro_pattern_template(p, patterns, templates);
            if (FuParser_check_token(p, TOK_CLOSE_BRACE)) {
                break;
            }
        }
        FuToken end_tok = FuParser_expect_token(p, TOK_CLOSE_BRACE);
        hi = end_tok.sp;
    } else if (FuParser_check_token_fn(p, FuToken_is_open_delim)) {
        FuParser_parse_macro_pattern_template(p, patterns, templates);
        FuVec *last_template = FuVec_last_ptr(templates);
        FuToken *last_tok = FuVec_last(last_template);
        hi = last_tok->sp;
    } else {
        FuToken err_tok = FuParser_nth_token(p, 0);
        FATAL1(err_tok.sp, "expect `(`, `[`, `{`, `=>`, find: `%s`", FuToken_kind_csr(err_tok));
    }
    FuSpan *sp = FuSpan_join(lo, hi);
    FuNode *nd = FuNode_new(p->pkg, sp, ND_MACRO_DEF);
    nd->attrs = attrs;
    nd->_macro_def.vis = vis;
    nd->_macro_def.ident = ident;
    nd->_macro_def.patterns = patterns;
    nd->_macro_def.templates = templates;
    return nd;
}

FuMacroCall *FuParser_parse_prefix_macro_call(FuParser *p) {
    FuPath *path = FuParser_parse_path(p);
    if (!path->is_macro) {
        FATAL(path->sp, "expect a macro path");
    }
    if (!FuParser_check_token_fn(p, FuToken_is_open_delim)) {
        FuToken err_tok = FuParser_nth_token(p, 0);
        FATAL1(err_tok.sp, "expect `(`, `[`, `{`, find: `%s`", FuToken_kind_csr(err_tok));
    }
    FuVec *tokens = FuVec_new(sizeof(FuToken));
    FuParser_parse_tok_group(p, tokens);
    FuToken *end_tok = FuVec_last(tokens);
    FuSpan *sp = FuSpan_join(path->sp, end_tok->sp);
    FuMacroCall *call = FuMacroCall_new(sp, FU_FALSE, path);
    call->left = NULL;
    call->args = tokens;
    return call;
}

void FuParser_parse_pkg(FuParser *p) {
    FuSpan *lo = FuParser_current_span(p);
    FuVec *attrs = FuVec_new(sizeof(FuAttr *));
    FuParser_parse_inner_attrs(p, attrs);
    FuVec *items = FuParser_parse_mod_items(p, attrs, TOK_EOF);
    FuSpan *hi = FuParser_current_span(p);
    FuSpan *sp = FuSpan_join(lo, hi);
    p->pkg->sp = sp;
    p->pkg->attrs = attrs;
    p->pkg->name = KW_DOLLAR_PKG;
    p->pkg->items = items;
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

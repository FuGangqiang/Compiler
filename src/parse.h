#ifndef FU_PARSE_H
#define FU_PARSE_H

#include "char.h"
#include "def.h"
#include "map.h"
#include "set.h"
#include "str.h"
#include "vec.h"

typedef fu_size_t FuSymbol;

typedef enum fu_keyword_k fu_keyword_k;
typedef enum fu_token_k fu_token_k;

typedef struct FuSpan FuSpan;
typedef struct FuContext FuContext;
typedef struct FuToken FuToken;
typedef struct FuLexer FuLexer;

fu_bool_t FuSymbol_eq(FuSymbol *sym1, FuSymbol *sym2);
fu_size_t FuSymbol_hash(FuSymbol *sym);

enum fu_keyword_k {
#define KEYWORD(kd, str) kd,
#include "keyword.def"
#undef KEYWORD
    _KW_LAST_UNUSED,
};

enum fu_token_k {
#define TOKEN(kd, str) kd,
#include "token.def"
#undef TOKEN
    _TOK_LAST_UNUSED
};

char *FuKind_keyword_cstr(fu_token_k kd);
char *FuKind_token_cstr(fu_token_k kd);

struct FuSpan {
    FuContext *ctx;
    FuSymbol fpath;
    fu_size_t start;
    fu_size_t len;
    fu_size_t line;
    fu_size_t column;
    fu_size_t offset; /* offset in token */
};

FuSpan FuSpan_new(FuContext *ctx, FuSymbol fpath, fu_size_t start, fu_size_t len, fu_size_t line, fu_size_t column);
FuSpan FuSpan_offset(FuSpan span, fu_size_t offset);
FuStr *FuSpan_display(FuSpan span);
FuSpan FuSpan_join(FuSpan span1, FuSpan span2);
int FuSpan_print(FILE *out, FuSpan span);
int FuSpan_print_line(FILE *out, FuSpan span);

struct FuContext {
    /* FuStr* */
    FuSet *symbols;

    /* fpath sym -> fcontent no*/
    FuMap *fmap;
    FuVec *fcontents;
};

FuContext *FuContext_new();
void FuContext_init(FuContext *ctx);
void FuContext_drop(FuContext *ctx);

FuSymbol FuContext_intern_symbol(FuContext *ctx, FuStr *symbol);
FuStr *FuContext_get_symbol(FuContext *ctx, FuSymbol sym);

void FuContext_intern_file(FuContext *ctx, FuSymbol fpath, FuStr *fcontent);
FuStr *FuContext_get_file(FuContext *ctx, FuSymbol fpath);

struct FuToken {
    fu_token_k kd;
    FuSpan span;
    union {
        FuSymbol sym;
        struct {
            FuSymbol sym;
            fu_bool_t terminated;
        } _byte;
        struct {
            FuSymbol sym;
            fu_bool_t terminated;
        } _char;
        struct {
            FuSymbol sym;
            fu_size_t base;
            fu_bool_t empty_int;
            fu_size_t suffix_start;
        } _int;
        struct {
            FuSymbol sym;
            fu_size_t base;
            fu_bool_t empty_exponent;
            fu_size_t suffix_start;
        } _float;
        struct {
            FuSymbol sym;
            fu_size_t n_hashes;
            /* 是否有第一个引号, todo: 查看是否需要这个属性 */
            fu_bool_t started;
            fu_size_t prefix_ignore;
            fu_bool_t terminated;
        } _str;
    };
};

FuToken FuToken_new(fu_token_k kd, FuSpan span);
FuToken FuToken_new_sym(fu_token_k kd, FuSpan span, FuSymbol sym);
FuToken FuToken_new_lit_int(FuSpan span, FuSymbol sym, fu_size_t base, fu_bool_t empty_int, fu_size_t suffix_start);
FuToken FuToken_new_lit_float(FuSpan span, FuSymbol sym, fu_size_t base, fu_bool_t empty_exponent,
                              fu_size_t suffix_start);
FuToken FuToken_new_lit_char(FuSpan span, FuSymbol sym, fu_bool_t terminated);
FuToken FuToken_new_lit_byte(FuSpan span, FuSymbol sym, fu_bool_t terminated);
FuToken FuToken_new_lit_str(fu_token_k kd, FuSpan span, FuSymbol sym, fu_size_t n_hashes, fu_bool_t started,
                            fu_size_t prefix_ignore, fu_bool_t terminated);

fu_bool_t FuToken_is_eof(FuToken tok);
fu_bool_t FuToken_is_keyword(FuToken tok);
fu_bool_t FuToken_is_ident(FuToken tok);
fu_bool_t FuToken_is_open_delim(FuToken tok);
fu_bool_t FuToken_is_close_delim(FuToken tok);
fu_bool_t FuToken_is_match_delim(FuToken open, FuToken close);
fu_bool_t FuToken_is_bin_op(FuToken tok);
fu_bool_t FuToken_is_bin_eq_op(FuToken tok);
fu_bool_t FuToken_is_lit(FuToken tok);
fu_bool_t FuToken_is_outer_doc_comment(FuToken tok);
fu_bool_t FuToken_is_blank(FuToken tok);

fu_size_t FuToken_left_skip_count(FuToken tok);

FuStr *FuToken_display(FuToken tok);

struct FuLexer {
    FuContext *ctx;
    FuSymbol fpath;
    /* interned fcontent */
    FuStr *chars;
    fu_size_t tok_line;
    fu_size_t tok_column;
    fu_size_t cur_line;
    fu_size_t cur_column;
    fu_size_t cursor;
};

FuLexer *FuLexer_new(FuContext *ctx);
void FuLexer_drop(FuLexer *l);

void FuLexer_for_file(FuLexer *l, char *fname, fu_size_t len);

fu_bool_t FuLexer_is_eof(FuLexer *l);
FuToken FuLexer_get_token(FuLexer *l);
FuStr *FuLexer_display_token(FuLexer *l, FuToken token);

FuStr *FuLexer_dump(FuLexer *l);

#endif /* FU_PARSE_H */

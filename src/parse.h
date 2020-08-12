#ifndef FU_PARSE_H
#define FU_PARSE_H

#include "bytes.h"
#include "char.h"
#include "def.h"
#include "map.h"
#include "set.h"
#include "str.h"
#include "vec.h"

typedef fu_size_t fu_nid_t;
typedef fu_size_t fu_tid_t;

typedef fu_size_t FuSymbol;

typedef enum fu_arm_k fu_arm_k;
typedef enum fu_attr_k fu_attr_k;
typedef enum fu_expr_k fu_expr_k;
typedef enum fu_ge_arg_k fu_ge_arg_k;
typedef enum fu_ge_param_k fu_ge_param_k;
typedef enum fu_keyword_k fu_keyword_k;
typedef enum fu_lit_k fu_lit_k;
typedef enum fu_node_k fu_node_k;
typedef enum fu_op_k fu_op_k;
typedef enum fu_pat_k fu_pat_k;
typedef enum fu_token_k fu_token_k;
typedef enum fu_type_k fu_type_k;
typedef enum fu_use_k fu_use_k;
typedef enum fu_variant_k fu_variant_k;
typedef enum fu_vis_k fu_vis_k;

typedef struct FuSpan FuSpan;
typedef struct FuContext FuContext;
typedef struct FuToken FuToken;
typedef struct FuLexer FuLexer;
typedef struct FuScope FuScope;

typedef struct FuType FuType;
typedef struct FuNode FuNode;

typedef struct FuAnnoSelf FuAnnoSelf;
typedef struct FuAttr FuAttr;
typedef struct FuExpr FuExpr;
typedef struct FuGeArg FuGeArg;
typedef struct FuGeBound FuGeBound;
typedef struct FuGeneric FuGeneric;
typedef struct FuGeParam FuGeParam;
typedef struct FuIdent FuIdent;
typedef struct FuLabel FuLabel;
typedef struct FuLit FuLit;
typedef struct FuMacroCall FuMacroCall;
typedef struct FuPat FuPat;
typedef struct FuPath FuPath;
typedef struct FuPathItem FuPathItem;
typedef struct FuPkg FuPkg;
typedef struct FuTokTree FuTokTree;
typedef struct FuUse FuUse;

fu_bool_t FuSymbol_eq(FuSymbol *sym1, FuSymbol *sym2);
fu_size_t FuSymbol_hash(FuSymbol *sym);

enum fu_arm_k {
#define ARM(kd, comment) kd,
#include "node_arm.def"
#undef ARM
    _ARM_LAST_UNUSED
};

enum fu_attr_k {
#define ATTR(kd, str) kd,
#include "node_attr.def"
#undef ATTR
    _ATTR_LAST_UNUSED
};

enum fu_expr_k {
#define EXPR(kd, str) kd,
#include "node_expr.def"
#undef EXPR
    _EXPR_LAST_UNUSED
};

enum fu_ge_arg_k {
#define GE_ARG(kd, str) kd,
#include "node_ge_arg.def"
#undef GE_ARG
    _GE_ARG_LAST_UNUSED
};

enum fu_ge_param_k {
#define GE_PARAM(kd, str) kd,
#include "node_ge_param.def"
#undef GE_PARAM
    _GE_PARAM_LAST_UNUSED,
};

enum fu_keyword_k {
#define KEYWORD(kd, str) kd,
#include "keyword.def"
#undef KEYWORD
    _KW_LAST_UNUSED,
};

enum fu_lit_k {
#define LIT(kd, str) kd,
#include "node_lit.def"
#undef LIT
    _LIT_LAST_UNUSED
};

enum fu_node_k {
#define NODE(kd, str) kd,
#include "node.def"
#undef NODE
    _ND_LAST_UNUSED
};

enum fu_op_k {
#define OP(kd, str) kd,
#include "node_op.def"
#undef OP
    _OP_LAST_UNUSED
};

enum fu_pat_k {
#define PAT(kd, str) kd,
#include "node_pat.def"
#undef PAT
    _PAT_LAST_UNUSED
};

enum fu_token_k {
#define TOKEN(kd, str) kd,
#include "token.def"
#undef TOKEN
    _TOK_LAST_UNUSED
};

enum fu_type_k {
#define TYPE(kd, str) kd,
#include "type.def"
#undef TYPE
    _TY_LAST_UNUSED,
};

enum fu_use_k {
#define USE(kd, str) kd,
#include "node_use.def"
#undef USE
    _USE_LAST_UNUSE
};

enum fu_variant_k {
#define VARIANT(kd, str) kd,
#include "node_variant.def"
#undef VARIANT
    _VARIANT_LAST_UNUSE
};

enum fu_vis_k {
#define VIS(kd, str) kd,
#include "node_vis.def"
#undef VIS
    _VIS_LAST_UNUSE
};

char *FuKind_arm_cstr(fu_arm_k kd);
char *FuKind_attr_cstr(fu_attr_k kd);
char *FuKind_expr_cstr(fu_expr_k kd);
char *FuKind_ge_arg_cstr(fu_ge_arg_k kd);
char *FuKind_ge_param_cstr(fu_ge_param_k kd);
char *FuKind_keyword_cstr(fu_keyword_k kd);
char *FuKind_lit_cstr(fu_lit_k kd);
char *FuKind_node_cstr(fu_node_k kd);
char *FuKind_op_cstr(fu_op_k kd);
char *FuKind_pat_cstr(fu_pat_k kd);
char *FuKind_token_cstr(fu_token_k kd);
char *FuKind_type_cstr(fu_type_k kd);
char *FuKind_use_cstr(fu_use_k kd);
char *FuKind_variant_cstr(fu_variant_k kd);
char *FuKind_vis_cstr(fu_vis_k kd);

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
    /* intern symbols */
    /* FuStr* */
    FuSet *symbols;

    /* intern lexer file context */
    /* fpath sym -> fcontent no*/
    FuMap *fmap;
    FuVec *fcontents;

    /* ast nodes */
    FuVec *nodes;
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

struct FuScope {
    FuContext *ctx;
    FuScope *super;
    FuSymbol name;

    fu_bool_t is_fn;
    FuMap *closure_syms;

    FuMap *declares;
    FuMap *labels;
    FuMap *types;
    FuMap *interfaces;
    FuMap *extensions;
};

FuScope *FuScope_new(FuContext *ctx, FuScope *super, FuSymbol name);
void FuScope_drop(FuScope *scp);

FuType *FuScope_get_type(FuScope *scp, FuSymbol name);

struct FuType {
    fu_type_k kd;
    fu_tid_t tid;
    fu_nid_t nid;
    fu_vis_k vis;
    fu_bool_t is_infered;
    FuVec *attrs;
    union {
        struct {
            FuGeneric *ge;
            fu_bool_t is_unsafe;
            fu_bool_t is_const;
            fu_bool_t is_async;
            FuVec *tys;
        } _fn;
        FuType *_ptr;
        struct {
            FuType *ty;
            FuExpr *size_expr;
        } _array;
        FuType *_slice;
        FuVec *_tuple;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._field_def */
            FuVec *items;
        } _struct;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuType */
            FuVec *items;
        } _tuple_struct;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._variant */
            FuVec *items;
        } _enum;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            /* FuNode._field_def */
            FuVec *items;
        } _union;
        struct {
            FuGeneric *ge;
            fu_bool_t is_unsafe;
            FuIdent *ident;
            /* FuNode._... */
            FuVec *assocs;
        } _interface;
        struct {
            FuAnnoSelf *anno;
            FuPath *path;
        } _path;
        struct {
            FuGeneric *ge;
            FuIdent *ident;
            FuType *ty;
        } _alias;
    };
};

struct FuTokTree {
    FuSpan span;
    fu_bool_t is_group;
    union {
        FuToken _token;
        struct {
            FuSpan open_span;
            FuSpan close_span;
            FuChar delimiter;
            /* FuTokTree */
            FuVec *tokens;
        } _group;
    };
};

struct FuIdent {
    FuSpan span;
    FuSymbol name;
};

struct FuLabel {
    FuSpan span;
    FuSymbol name;
};

struct FuAttr {
    fu_attr_k kd;
    fu_bool_t is_outer;
    union {
        struct {
            FuPath *path;
            FuTokTree *tok_tree;
        } _normal;
        FuStr *_doc_comment;
    };
};

struct FuUse {
    FuSpan span;
    FuStr *prefix;
    fu_use_k kd;
    union {
        FuNode *_alias;
        /* FuUse */
        FuVec *_nested;
    };
};

/* `std`, `Vec#<T>` */
struct FuPathItem {
    FuSpan span;
    FuIdent *ident;
    /* FuGeArg */
    FuVec *ge_args;
};

/* `std::vec::Vec#<T>` */
struct FuPath {
    FuSpan span;
    /* FuPathItem */
    FuVec *segments;
};

/*
 * 类型注解
 * 和 `FuPath` 一起使用
 * `idx` 代表 `Fupath` 里面的索引
 *
 * ```
 * <Vec#<T> as a::b::IInterface>::AssocItem
 *  ^~~~~     ~~~~~~~~~~~~~~^
 *  ty        idx = 3
 *
 * Vec#<T>::AssocItem
 *  ^~~~~    ^
 *  ty       idx = 0
 * ```
 */
struct FuAnnoSelf {
    FuSpan span;
    FuType *ty;
    fu_size_t idx;
};

struct FuMacroCall {
    fu_bool_t is_method;
    FuNode *path;
    FuTokTree *args;
};

struct FuGeParam {
    FuSpan span;
    fu_ge_param_k kd;
    FuIdent *ident;
    union {
        struct {
            FuType *deflt;
        } _type;
        struct {
            FuType *ty;
        } _const;
    };
};

struct FuGeBound {
    FuSpan span;
    FuType *ty;
    /* FuType */
    FuVec *interfaces;
};

struct FuGeneric {
    /* FuGeParam */
    FuVec *params;
    /* FuGeBound */
    FuVec *bounds;
};

struct FuGeArg {
    FuSpan span;
    fu_ge_arg_k kd;
    union {
        FuType *_type;
        FuNode *_const;
        struct {
            FuType *param;
            FuType *ty;
        } _binding;
    };
};

struct FuLit {
    FuSpan span;
    fu_lit_k kd;
    union {
        struct {
            fu_uint8_t v;
        } _bool;
        struct {
            fu_uint8_t v;
        } _byte;
        struct {
            fu_uint32_t v;
        } _char;
        struct {
            /* 0: unsuffix, 8, 16, 32, 64 */
            fu_uint8_t size;
            fu_bool_t is_signed;
            fu_uint64_t v;
        } _int;
        struct {
            FuToken tok;
            /* 0: unsuffix, 32, 64 */
            fu_uint8_t size;
            union {
                float v32;
                double v64;
            };
        } _float;
        FuStr *_str;
        FuBytes *_byte_str;
        FuStr *_format_str;
    };
};

FuLit *FuLit_new(FuSpan span, fu_lit_k kind);

struct FuExpr {
    FuSpan span;
    fu_expr_k kd;
    fu_bool_t is_const;
    union {
        struct {
            FuAnnoSelf *anno;
            FuPath *path;
        } _path;
        struct {
            /* FuNode._field_init */
            FuVec *fields;
            FuNode *base_expr;
            fu_size_t size;
        } _array;
        struct {
            /* FuNode._field_init */
            FuVec *fields;
            FuNode *base_expr;
        } _tuple;
        struct {
            FuPath *path;
            /* FuNode._field_init */
            FuVec *fields;
            FuNode *base_expr;
        } _struct;
        struct {
            fu_bool_t is_inclusive;
            FuNode *start_expr;
            FuNode *stop_expr;
        } _range;
        struct {
            FuNode *obj_expr;
            FuIdent *ident;
        } _field;
        struct {
            FuNode *obj_expr;
            FuNode *idx_expr;
        } _index;
        struct {
            FuNode *expr;
        } _address_of;
        struct {
            FuNode *expr;
            FuType *ty;
        } _cast;
        struct {
            FuNode *path;
            /* FuNode._expr */
            FuVec *expr_args;
        } _call;
        struct {
            FuNode *path;
            /* FuNode._expr */
            FuVec *expr_args;
        } _method_call;
        struct {
            fu_op_k op;
            FuNode *expr;
        } _unary;
        struct {
            fu_op_k op;
            FuNode *lexpr;
            FuNode *rexpr;
        } _binary;
        struct {
            FuNode *lexpr;
            FuNode *rexpr;
        } _assign;
        struct {
            fu_op_k op;
            FuNode *lexpr;
            FuNode *rexpr;
        } _assign_op;
        struct {
            FuLabel *label;
            FuNode *expr;
        } _break;
        struct {
            FuLabel *label;
        } _continue;
        struct {
            FuNode *expr;
        } _yield;
        struct {
            FuNode *expr;
        } _throw;
        struct {
            FuNode *expr;
        } _return;
        struct {
            FuNode *expr;
        } _await;
        struct {
            fu_bool_t is_unsafe;
            fu_bool_t is_async;
            FuScope *scope;
            FuLabel *label;
            /* FuNode */
            FuVec *items;
        } _block;
        struct {
            FuScope *scope;
            /* FuNode._fn_param */
            FuVec *params;
            FuNode *body;
        } _closure;
        struct {
            FuPat *pat;
            FuNode *expr;
        } _let_cond;
        struct {
            FuNode *cond_expr;
            FuNode *block;
            FuNode *next_if;
        } _if;
        struct {
            FuNode *cond_expr;
            FuNode *block;
        } _while;
        struct {
            FuNode *cond_expr;
            FuNode *block;
        } _do_while;
        struct {
            FuPat *pat;
            FuNode *expr;
            FuNode *block;
        } _for;
        struct {
            FuNode *block;
        } _loop;
        struct {
            FuNode *expr;
            /* FuNode._arm */
            FuVec *arms;
        } _match;
        struct {
            FuNode *block;
            /* FuNode._arm */
            FuVec *arms;
            FuNode *finally;
        } _try;
        FuMacroCall *_macro_call;
    };
};

struct FuPat {
    FuSpan span;
    fu_pat_k kd;
    union {
        FuLit *_lit;
        struct {
            FuAnnoSelf *anno;
            FuPath *path;
        } _path;
        FuExpr *_range;
        struct {
            FuVec *pats;
        } _or;
        struct {
            FuVec *pats;
        } _tuple;
        struct {
            FuVec *pats;
        } _slice;
        struct {
            FuExpr *path;
            FuVec *fields;
        } _struct;
        struct {
            FuIdent *ident;
            FuPat *pat;
        } _field;
        struct {
            FuExpr *path;
            FuVec *pats;
        } _tuple_struct;
        struct {
            FuExpr *expr;
        } _repeat;
        struct {
            FuExpr *expr;
        } _base;
        struct {
            fu_bool_t is_ref;
            FuIdent *ident;
            FuPat *pat;
        } _binding;
    };
};

/*
 * Node 结构里面有：
 * - 顶层 item 结构
 *     - use
 *     - static
 *     - const
 *     - type
 *     - mod
 *     - extension
 *     - macro def
 *     - macro call
 * - 有 attr 的结构，便于以后对 attr 统一分析
 *     - fn_param
 *     - arm
 *     - field_def
 *     - variant
 *     - pat
 * - 有类型的结构，便于以后对类型统一分析
 *     - 顶层 expr
 *     - field_init
 */
struct FuNode {
    FuSpan span;
    fu_node_k kd;
    /* FuAttr */
    FuVec *attrs;
    fu_nid_t nid;
    fu_bool_t is_inferred;
    union {
        struct {
            FuType *ty;
            FuLit *lit;
        } _lit;
        struct {
            FuType *ty;
            FuExpr *expr;
        } _expr;
        struct {
            fu_vis_k vis;
            FuUse *tree;
        } _use;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
            FuExpr *init_expr;
        } _static;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
            FuExpr *init_expr;
        } _const;
        struct {
            FuPat *pat;
            FuType *ty;
            FuExpr *init_expr;
        } _let;
        struct {
            FuIdent *ident;
            FuScope *scope;
            FuType *ty;
            /* FuNode._fn_param */
            FuVec *params;
            /* FuNode */
            FuVec *body;
        } _fn;
        FuType *_type;
        struct {
            /* FuNode
             *     - _static
             *     - _const
             *     - _type
             *     - _fn
             */
            FuVec *declares;
        } _extern;
        struct {
            FuGeneric *ge;
            FuType *ty;
            FuType *interface;
            /* FuNode
             *     - _const
             *     - _fn
             *     - _type
             *     - _macro_call
             */
            FuVec *assocs;
        } _extension;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            fu_bool_t is_method;
            FuTokTree *args;
            /* FuTokTree */
            FuVec *patterns;
            /* FuTokTree */
            FuVec *templates;
        } _macro_def;
        FuMacroCall *_macro_call;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuScope *scope;
            /* inner file span` */
            FuSpan inner_span;
            /* flase for `mod name;` */
            fu_bool_t is_inline;
            /* FuNode */
            FuVec *items;
        } _mod;
        struct {
            FuNode *mod;

            FuVec *extern_pkgs;
            FuScope *builtins;
            FuScope *globals;
        } _pkg;
        struct {
            FuIdent *ident;
            FuType *ty;
        } _fn_param;
        struct {
            fu_arm_k kd;
            FuPat *pat;
            FuNode *guard_expr;
            FuNode *body;
        } _arm;
        FuPat *_pat;
        struct {
            fu_vis_k vis;
            FuIdent *ident;
            FuType *ty;
        } _field_def;
        struct {
            fu_variant_k kd;
            fu_vis_k vis;
            FuIdent *ident;
            /* `name: int` */
            FuVec *fields;
        } _variant;
        struct {
            FuIdent *ident;
            FuNode *_init_expr;
            FuType *ty;
        } _field_init;
    };
};

FuNode *FuNode_new(FuContext *ctx, FuSpan span, fu_node_k kind);
void FuNode_drop(FuNode *nd);

FuStr *FuNode_display(FuNode *nd, fu_size_t indent);

#endif /* FU_PARSE_H */

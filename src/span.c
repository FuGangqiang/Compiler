#include "alloc.h"
#include "log.h"
#include "parse.h"
#include "unix.h"

FuSpan *FuSpan_new(FuPkg *pkg, fu_sym_t fpath, fu_size_t start, fu_size_t len, fu_size_t line, fu_size_t column) {
    FuSpan *sp = FuMem_new(FuSpan);
    FuSpan_init(sp, pkg, fpath, start, len, line, column);
    FuPkg_intern_span(pkg, sp);
    return sp;
}

void FuSpan_init(FuSpan *sp, FuPkg *pkg, fu_sym_t fpath, fu_size_t start, fu_size_t len, fu_size_t line,
                 fu_size_t column) {
    sp->pkg = pkg;
    sp->fpath = fpath;
    sp->start = start;
    sp->len = len;
    sp->line = line;
    sp->column = column;
    sp->offset = 0;
}

void FuSpan_drop(FuSpan *sp) {
    FuMem_free(sp);
}

FuSpan *FuSpan_clone(FuSpan *sp) {
    FuSpan *new = FuMem_new(FuSpan);
    FuSpan_init(new, sp->pkg, sp->fpath, sp->start, sp->len, sp->line, sp->column);
    return new;
}

FuSpan *FuSpan_offset(FuSpan *sp, fu_size_t offset) {
    FuSpan *new = FuSpan_clone(sp);
    new->offset = offset;
    FuPkg_intern_span(new->pkg, new);
    return new;
}

FuSpan *FuSpan_unintern_join(FuSpan *sp1, FuSpan *sp2) {
    if (sp1->pkg != sp2->pkg) {
        FATAL(NULL, "span context not match");
    }
    if (sp1->fpath != sp2->fpath) {
        FATAL(NULL, "span fpath not match");
    }
    fu_size_t start, line, column;
    if (sp1->start < sp2->start) {
        start = sp1->start;
        line = sp1->line;
        column = sp1->column;
    } else {
        start = sp2->start;
        line = sp2->line;
        column = sp2->column;
    }
    fu_size_t end, end1, end2;
    end1 = sp1->start + sp1->len;
    end2 = sp2->start + sp2->len;
    if (end1 < end2) {
        end = end2;
    } else {
        end = end1;
    }
    FuSpan *new = FuMem_new(FuSpan);
    FuSpan_init(new, sp1->pkg, sp1->fpath, start, end - start, line, column);
    return new;
}

FuSpan *FuSpan_join(FuSpan *sp1, FuSpan *sp2) {
    FuSpan *new = FuSpan_unintern_join(sp1, sp2);
    FuPkg_intern_span(new->pkg, new);
    return new;
}

FuStr *FuSpan_display(FuSpan *sp) {
    fu_size_t line = sp->line;
    fu_size_t column = sp->column;

    if (sp->offset) {
        FuStr *fcontent = FuPkg_get_file(sp->pkg, sp->fpath);
        fu_size_t i;
        for (i = 0; i < sp->offset; i++) {
            if (FuStr_get_char(fcontent, sp->start + i) == '\n') {
                line++;
                column = 0;
            } else {
                column++;
            }
        }
    }

    FuStr *str = FuPkg_get_symbol(sp->pkg, sp->fpath);
    FuStr *display = FuStr_rel_path(str, sp->pkg->dir);
    FuStr_push_utf8_format(display, ":%u:%u", line, column);
    return display;
}

FuStr *FuSpan_line(FuSpan *sp) {
    FuStr *fcontent = FuPkg_get_file(sp->pkg, sp->fpath);
    fu_size_t len = FuStr_len(fcontent);

    fu_size_t start, end;
    start = end = sp->start + sp->offset;

    /* EOF */
    if (start >= len) {
        return 0;
    }

    while (start != 0) {
        if (FuStr_get_char(fcontent, start) == '\n') {
            start++;
            break;
        }
        start--;
    }
    while (end != len) {
        if (end >= len) {
            break;
        }
        if (FuStr_get_char(fcontent, end) == '\n') {
            end++;
            break;
        }
        end++;
    }
    return FuStr_from_slice(fcontent, start, end - start);
}

FuStr *FuSpan_content(FuSpan *sp) {
    FuStr *fcontent = FuPkg_get_file(sp->pkg, sp->fpath);
    return FuStr_from_slice(fcontent, sp->start, sp->len);
}

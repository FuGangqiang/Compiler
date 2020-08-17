#include "error.h"
#include "parse.h"

FuSpan FuSpan_new(FuCtx *ctx, fu_sym_t fpath, fu_size_t start, fu_size_t len, fu_size_t line, fu_size_t column) {
    FuSpan span;
    span.ctx = ctx;
    span.fpath = fpath;
    span.start = start;
    span.len = len;
    span.line = line;
    span.column = column;
    span.offset = 0;
    return span;
}

FuSpan FuSpan_offset(FuSpan span, fu_size_t offset) {
    FuSpan new = span;
    new.offset = offset;
    return new;
}

FuStr *FuSpan_display(FuSpan span) {
    fu_size_t line = span.line;
    fu_size_t column = span.column;

    if (span.offset) {
        FuStr *fcontent = FuCtx_get_file(span.ctx, span.fpath);
        fu_size_t i;
        for (i = 0; i < span.offset; i++) {
            if (FuStr_get_char(fcontent, span.start + i) == '\n') {
                line++;
                column = 0;
            } else {
                column++;
            }
        }
    }

    FuStr *str = FuCtx_get_symbol(span.ctx, span.fpath);
    FuStr *display = FuStr_clone(str);
    FuStr_push_utf8_format(display, ":%u:%u", line, column);
    return display;
}

FuSpan FuSpan_join(FuSpan span1, FuSpan span2) {
    if (span1.ctx != span2.ctx) {
        FATAL(NULL, "span context not match");
    }
    if (span1.fpath != span2.fpath) {
        FATAL(NULL, "span fpath not match");
    }
    fu_size_t start, line, column;
    if (span1.start < span2.start) {
        start = span1.start;
        line = span1.line;
        column = span1.column;
    } else {
        start = span2.start;
        line = span2.line;
        column = span2.column;
    }
    fu_size_t end, end1, end2;
    end1 = span1.start + span1.len;
    end2 = span2.start + span2.len;
    if (end1 < end2) {
        end = end2;
    } else {
        end = end1;
    }
    return FuSpan_new(span1.ctx, span1.fpath, start, end - start, line, column);
}

int FuSpan_print(FILE *out, FuSpan span) {
    FuStr *str = FuSpan_display(span);
    int count = FuStr_print(out, str);
    FuStr_drop(str);
    return count;
}

int FuSpan_print_line(FILE *out, FuSpan span) {
    FuStr *fcontent = FuCtx_get_file(span.ctx, span.fpath);
    fu_size_t len = FuStr_len(fcontent);

    fu_size_t start, end;
    start = end = span.start + span.offset;

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
    FuStr *str = FuStr_from_slice(fcontent, start, end - start);
    fprintf(out, "\n");
    int count = FuStr_print(out, str);
    fprintf(out, "\n");
    FuStr_drop(str);
    return count;
}

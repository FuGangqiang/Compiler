#include "error.h"

void warning_format(FuSpan *sp, const char *format, ...) {
    va_list ap;

    if (sp) {
        FuSpan_print(stderr, sp);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "warning: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (sp) {
        FuSpan_print_line(stderr, sp);
    }

    return;
}

void error_format(FuSpan *sp, const char *format, ...) {
    va_list ap;

    if (sp) {
        FuSpan_print(stderr, sp);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "error: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (sp) {
        FuSpan_print_line(stderr, sp);
    }

    return;
}

void fatal_format(FuSpan *sp, const char *format, ...) {
    va_list ap;

    if (sp) {
        FuSpan_print(stderr, sp);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "fatal: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (sp) {
        FuSpan_print_line(stderr, sp);
    }

    exit(1);
}

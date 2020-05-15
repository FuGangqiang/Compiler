#include "error.h"

void warning_format(FuSpan *span, const char *format, ...) {
    va_list ap;

    if (span) {
        FuSpan_print(stderr, *span);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "warning: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (span) {
        FuSpan_print_line(stderr, *span);
    }

    return;
}

void error_format(FuSpan *span, const char *format, ...) {
    va_list ap;

    if (span) {
        FuSpan_print(stderr, *span);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "error: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (span) {
        FuSpan_print_line(stderr, *span);
    }

    return;
}

void fatal_format(FuSpan *span, const char *format, ...) {
    va_list ap;

    if (span) {
        FuSpan_print(stderr, *span);
        fprintf(stderr, ": ");
    }

    fprintf(stderr, "fatal: ");
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\n");
    va_end(ap);

    if (span) {
        FuSpan_print_line(stderr, *span);
    }

    exit(1);
}

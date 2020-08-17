#ifndef FU_ERROR_H
#define FU_ERROR_H

#include <stdio.h>

#include "parse.h"

void warning_format(FuSpan *sp, const char *format, ...);
void error_format(FuSpan *sp, const char *format, ...);
void fatal_format(FuSpan *sp, const char *format, ...);

#define WARNING fprintf(stderr, "[%s:%d]: ", __FILE__, __LINE__), warning_format
#define ERROR fprintf(stderr, "[%s:%d]: ", __FILE__, __LINE__), error_format
#define FATAL fprintf(stderr, "[%s:%d]: ", __FILE__, __LINE__), fatal_format

#endif /* FU_ERROR_H */

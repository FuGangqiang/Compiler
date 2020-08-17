#include "log.h"
#include "alloc.h"

FuLog *FuLog_new(fu_log_k kd, FuSpan *sp, FuStr *msg) {
    FuLog *log = FuMem_new(FuLog);
    log->kd = kd;
    log->sp = sp;
    log->msg = msg;
    return log;
}

void FuLog_drop(FuLog *log) {
    if (!log) {
        return;
    }
    FuStr_drop(log->msg);
    FuMem_free(log);
}

FuStr *FuLog_dispaly(FuLog *log) {
    FuStr *str = FuStr_new();
    if (log->file) {
        FuStr_push_utf8_format(str, "[%s:%d]: ", log->file, log->line);
    }
    if (log->sp) {
        FuStr_append(str, FuSpan_display(log->sp));
        FuStr_push_utf8_cstr(str, " :");
    }
    FuStr_push_utf8_cstr(str, FuKind_log_cstr(log->kd));
    FuStr_push_utf8_cstr(str, ": ");
    FuStr_append(str, FuStr_clone(log->msg));
    FuStr_push(str, '\n');
    if (log->sp) {
        FuStr_append(str, FuSpan_line(log->sp));
    }
    return str;
}

void FuLog_print(FuLog *log) {
    FuStr *display = FuLog_dispaly(log);
    if (log->kd >= LOG_WARN) {
        FuStr_print(stderr, display);
    } else {
        FuStr_print(stdout, display);
    }
    if (log->kd == LOG_FATAL) {
        exit(1);
    }
}

void FuLog_format(char *file, fu_size_t line, fu_log_k kd, FuSpan *sp, const char *format, ...) {
    FuStr *msg = FuStr_new();

    va_list ap;
    va_start(ap, format);
    FuStr_push_utf8_format_v(msg, format, ap);
    va_end(ap);

    if (FuStr_last_char(msg) != '\n') {
        FuStr_push(msg, '\n');
    }
    FuLog *log = FuLog_new(kd, sp, msg);
    /* todo: use macro to disable */
    log->file = file;
    log->line = line;
    FuLog_print(log);
    FuLog_drop(log);
}

#ifndef Fu_LOG_H
#define Fu_LOG_H

#include "parse.h"

struct FuLog {
    fu_log_k kd;
    FuStr *msg;
    FuSpan *sp;
    /* compiler src file & line */
    char *file;
    fu_size_t line;
};

FuLog *FuLog_new(fu_log_k kd, FuSpan *sp, FuStr *msg);
void FuLog_drop(FuLog *log);
FuStr *FuLog_dispaly(FuLog *log);
void FuLog_print(FuLog *log);
void FuLog_format(char *file, fu_size_t line, fu_log_k kd, FuSpan *sp, const char *format, ...);

#define __FL__ __FILE__, __LINE__

/* c89 can not use __VA_ARGS__ in c99 */
#define DEBUG(sp, fmt) FuLog_format(__FL__, LOG_DEBUG, sp, fmt)
#define DEBUG1(sp, fmt, a1) FuLog_format(__FL__, LOG_DEBUG, sp, fmt, a1)
#define DEBUG2(sp, fmt, a1, a2) FuLog_format(__FL__, LOG_DEBUG, sp, fmt, a1, a2)
#define DEBUG3(sp, fmt, a1, a2, a3) FuLog_format(__FL__, LOG_DEBUG, sp, fmt, a1, a2, a3)

/* c89 can not use __VA_ARGS__ in c99 */
#define INFO(sp, fmt) FuLog_format(__FL__, LOG_INFO, sp, fmt)
#define INFO1(sp, fmt, a1) FuLog_format(__FL__, LOG_INFO, sp, fmt, a1)
#define INFO2(sp, fmt, a1, a2) FuLog_format(__FL__, LOG_INFO, sp, fmt, a1, a2)
#define INFO3(sp, fmt, a1, a2, a3) FuLog_format(__FL__, LOG_INFO, sp, fmt, a1, a2, a3)

/* c89 can not use __VA_ARGS__ in c99 */
#define WARN(sp, fmt) FuLog_format(__FL__, LOG_WARN, sp, fmt)
#define WARN1(sp, fmt, a1) FuLog_format(__FL__, LOG_WARN, sp, fmt, a1)
#define WARN2(sp, fmt, a1, a2) FuLog_format(__FL__, LOG_WARN, sp, fmt, a1, a2)
#define WARN3(sp, fmt, a1, a2, a3) FuLog_format(__FL__, LOG_WARN, sp, fmt, a1, a2, a3)

/* c89 can not use __VA_ARGS__ in c99 */
#define ERROR(sp, fmt) FuLog_format(__FL__, LOG_ERROR, sp, fmt)
#define ERROR1(sp, fmt, a1) FuLog_format(__FL__, LOG_ERROR, sp, fmt, a1)
#define ERROR2(sp, fmt, a1, a2) FuLog_format(__FL__, LOG_ERROR, sp, fmt, a1, a2)
#define ERROR3(sp, fmt, a1, a2, a3) FuLog_format(__FL__, LOG_ERROR, sp, fmt, a1, a2, a3)

/* c89 can not use __VA_ARGS__ in c99 */
#define FATAL(sp, fmt) FuLog_format(__FL__, LOG_FATAL, sp, fmt)
#define FATAL1(sp, fmt, a1) FuLog_format(__FL__, LOG_FATAL, sp, fmt, a1)
#define FATAL2(sp, fmt, a1, a2) FuLog_format(__FL__, LOG_FATAL, sp, fmt, a1, a2)
#define FATAL3(sp, fmt, a1, a2, a3) FuLog_format(__FL__, LOG_FATAL, sp, fmt, a1, a2, a3)

#endif /* Fu_LOG_H */

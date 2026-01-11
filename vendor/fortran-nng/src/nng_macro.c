/* Macro replacements and wrappers around variadic functions. */
#include <nng/nng.h>

#ifdef __cplusplus
extern "C" {
#endif

void nng_log_auth_(nng_log_level, const char *, const char *);
void nng_log_debug_(const char *, const char *);
void nng_log_err_(const char *, const char *);
void nng_log_info_(const char *, const char *);
void nng_log_notice_(const char *, const char *);
void nng_log_warn_(const char *, const char *);

void nng_log_auth_(nng_log_level level, const char *msgid, const char *msg)
{
    nng_log_auth(level, msgid, "%s", msg);
}

void nng_log_debug_(const char *msgid, const char *msg)
{
    nng_log_debug(msgid, "%s", msg);
}

void nng_log_err_(const char *msgid, const char *msg)
{
    nng_log_err(msgid, "%s", msg);
}

void nng_log_info_(const char *msgid, const char *msg)
{
    nng_log_info(msgid, "%s", msg);
}

void nng_log_notice_(const char *msgid, const char *msg)
{
    nng_log_notice(msgid, "%s", msg);
}

void nng_log_warn_(const char *msgid, const char *msg)
{
    nng_log_warn(msgid, "%s", msg);
}

#ifdef __cplusplus
}
#endif

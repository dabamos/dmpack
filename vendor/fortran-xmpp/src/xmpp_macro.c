/* xmpp_macro.c */
#include <strophe.h>

#ifdef __cplusplus
extern "C" {
#endif

void xmpp_send_raw_string_(xmpp_conn_t *, const char *);

/* Non-variadic wrappers. */
void xmpp_send_raw_string_(xmpp_conn_t *conn, const char *str)
{
    xmpp_send_raw_string(conn, "%s", str);
}

#ifdef __cplusplus
}
#endif


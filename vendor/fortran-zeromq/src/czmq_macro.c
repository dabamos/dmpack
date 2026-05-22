/* czmq_macro.c */

#include <czmq.h>
#include <zsock.h>

#ifdef __cplusplus
extern "C" {
#endif

int zsock_bind_(void *, const char *);
int zsock_brecv_(void *, const char *);
int zsock_bsend_(void *, const char *);
int zsock_connect_(void *, const char *);
int zsock_disconnect_(void *, const char *);
int zsock_recv_(void *, const char *);
int zsock_send_(void *, const char *);

int zsock_bind_(void *self, const char *str) {
    return zsock_bind((zsock_t *) self, "%s", str);
}

int zsock_brecv_(void *self, const char *str)
{
    return zsock_brecv(self, str);
}

int zsock_bsend_(void *self, const char *str)
{
    return zsock_bsend(self, str);
}

int zsock_connect_(void*self, const char *str)
{
    return zsock_connect((zsock_t *) self, "%s", str);
}

int zsock_disconnect_(void *self, const char *str)
{
    return zsock_disconnect((zsock_t *) self, "%s", str);
}

int zsock_recv_(void *self, const char *str)
{
    return zsock_recv(self, "%s", str);
}

int zsock_send_(void *self, const char *str)
{
    return zsock_send(self, "%s", str);
}

#ifdef __cplusplus
}
#endif

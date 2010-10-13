// -*- objc -*-

#include <stdio.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "HsFFI.h"
#include "erl_interface.h"

#ifdef __GLASGOW_HASKELL__
#endif

#define MAX_BUF 1024

void vaskerl_loop (int fd)
{
    unsigned char buf[MAX_BUF];

    while (1)
    {
        ErlMessage emsg;
        int got = erl_receive_msg(fd, buf, MAX_BUF, &emsg);

        if (got == ERL_TICK)
            continue;
        else if (got == ERL_ERROR)
        {
            fprintf(stderr, "erl_error\n");
            break;
        }

        if (emsg.type == ERL_REG_SEND)
        {
            printf("Got something...\n");

            ETERM *resp = erl_format("{cnode, \"hello\"}");
            erl_send(fd, emsg.from, resp);
            erl_free_term(resp);

            erl_free_term(emsg.from);
            erl_free_term(emsg.msg);
        }
    }
}

static int fd = -1;

//HsBool
HsInt vaskerl_init_connect (char *name, char *ip, char *host,
                            char *dest, char *cookie)
{
    if (fd > -1)
        return fd;

    struct in_addr addr;
    addr.s_addr = inet_addr(ip);

    // struct hostent *he = erl_gethostbyname("127.0.0.1");
    // printf("... %s\n", he->h_name);
    char fullname[256];
    sprintf(fullname, "%s@%s", name, host);

    erl_init(NULL, 0);
    if (erl_connect_xinit(host, name, fullname, &addr, cookie, 0) < 0)
        //if (erl_connect_xinit("McHoovy.rd.shawcable.net", "vaskerl",
        //                  "vaskerl@McHoovy.rd.shawcable.net", &addr,
        //                  cookie, 0) < 0)
        erl_err_quit("erl_connect_xinit failed");

    printf("node=%s, host=%s, alive=%s, creation=%d\n", 
           erl_thisnodename(), erl_thishostname(), 
           erl_thisalivename(), erl_thiscreation());

    if ((fd = erl_xconnect(&addr, dest)) < 0)
        erl_err_quit("erl_xconnect failed");

    return fd;
    //return HS_BOOL_TRUE;
}

/*
int main (int argc, char *argv[])
{
    hs_init(&argc, &argv);

    loop(fd);

    hs_exit();
    return 0;
}
*/

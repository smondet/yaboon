#! /bin/sh
tail -n +4 $0 >/tmp/cs.$$.c && gcc -O0 -pipe -o /tmp/cs.$$ /tmp/cs.$$.c && /tmp/cs.$$ $*; rm -f /tmp/cs.$$*; exit

/* #include <arpa/inet.h>    */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/tcp.h>
#include <linux/dccp.h>
/* #include <linux/in.h> */

#define SOCK_DCCP 6      
/* #define IPPROTO_DCCP 33  //it must be this number. This number is assigned by IANA to DCCP */
#define SOL_DCCP 269    

char *
downcase(char *str) {
    int c = 0;
    char * memory_leak = strdup(str);
    while (memory_leak[c] != '\0') {
        if (0x41 <= memory_leak[c] && memory_leak[c] <= 0x5A) {
            /* printf("%d\n", c); */
            memory_leak[c] += 32;
        }
        c++;
    }
    return memory_leak;
}


#define print_macro(m) do {\
    printf("    let %30s = %d\n", downcase(#m), m); \
} while (0)


int main () {
    print_macro(PF_INET);
    print_macro(PF_INET6);

    print_macro( PF_UNIX);
    print_macro( PF_LOCAL);
    print_macro( PF_IPX);
    print_macro( PF_NETLINK);
    print_macro( PF_X25);
    print_macro( PF_AX25);
    print_macro( PF_ATMPVC);
    print_macro( PF_APPLETALK);
    print_macro( PF_PACKET);
    print_macro( SOCK_STREAM);
    print_macro( SOCK_DCCP);
    print_macro( SOCK_SEQPACKET);
    print_macro( SOCK_RAW);
    print_macro( SOCK_RDM);
    print_macro( SOCK_PACKET);
    print_macro( TCP_CORK);
    print_macro( TCP_DEFER_ACCEPT);
    print_macro( TCP_INFO);
    print_macro( TCP_KEEPCNT);
    print_macro( TCP_KEEPIDLE);
    print_macro( TCP_KEEPINTVL);
    print_macro( TCP_LINGER2);
    print_macro( TCP_MAXSEG);
    print_macro( TCP_NODELAY);
    print_macro( TCP_QUICKACK);
    print_macro( TCP_SYNCNT);
    print_macro( TCP_WINDOW_CLAMP);
    print_macro( SOL_DCCP);
    print_macro( SO_REUSEADDR);

    print_macro( DCCP_SOCKOPT_PACKET_SIZE);
    print_macro( DCCP_SOCKOPT_SERVICE);
    print_macro( DCCP_SOCKOPT_CHANGE_L);
    print_macro( DCCP_SOCKOPT_CHANGE_R);
    print_macro( DCCP_SOCKOPT_SEND_CSCOV);
    print_macro( DCCP_SOCKOPT_RECV_CSCOV);
    print_macro( DCCP_SOCKOPT_CCID_RX_INFO);
    print_macro( DCCP_SOCKOPT_CCID_TX_INFO);


    int IPPROTO_IP = 0;  /* Dummy protocol for TCP  */
    int IPPROTO_ICMP = 1;  /* Internet Control Message Protocol */
    int IPPROTO_IGMP = 2;  /* Internet Group Management Protocol */
    int IPPROTO_IPIP = 4;  /* IPIP tunnels (older KA9Q tunnels use 94) */
    int IPPROTO_TCP = 6;  /* Transmission Control Protocol */
    int IPPROTO_EGP = 8;  /* Exterior Gateway Protocol  */
    int IPPROTO_PUP = 12;  /* PUP protocol    */
    int IPPROTO_UDP = 17;  /* User Datagram Protocol  */
    int IPPROTO_IDP = 22;  /* XNS IDP protocol   */
    int IPPROTO_DCCP = 33;  /* Datagram Congestion Control Protocol */
    int IPPROTO_RSVP = 46;  /* RSVP protocol   */
    int IPPROTO_GRE = 47;  /* Cisco GRE tunnels (rfc 1701,1702) */

    int IPPROTO_IPV6  = 41;  /* IPv6-in-IPv4 tunnelling  */

    int IPPROTO_ESP = 50;            /* Encapsulation Security Payload protocol */
    int IPPROTO_AH = 51;             /* Authentication Header protocol       */
    int IPPROTO_BEETPH = 94;        /* IP option pseudo header for BEET */
    int IPPROTO_PIM    = 103;  /* Protocol Independent Multicast */

    int IPPROTO_COMP   = 108;                /* Compression Header protocol */
    int IPPROTO_SCTP   = 132;  /* Stream Control Transport Protocol */
    int IPPROTO_UDPLITE = 136; /* UDP-Lite (RFC 3828)   */

    int IPPROTO_RAW  = 255;  /* Raw IP packets   */

    print_macro(IPPROTO_IP );  /* Dummy protocol for TCP  */
    print_macro(IPPROTO_ICMP );  /* Internet Control Message Protocol */
    print_macro(IPPROTO_IGMP );  /* Internet Group Management Protocol */
    print_macro(IPPROTO_IPIP );  /* IPIP tunnels (older KA9Q tunnels use 94) */
    print_macro(IPPROTO_TCP );  /* Transmission Control Protocol */
    print_macro(IPPROTO_EGP );  /* Exterior Gateway Protocol  */
    print_macro(IPPROTO_PUP );  /* PUP protocol    */
    print_macro(IPPROTO_UDP );  /* User Datagram Protocol  */
    print_macro(IPPROTO_IDP );  /* XNS IDP protocol   */
    print_macro(IPPROTO_DCCP );  /* Datagram Congestion Control Protocol */
    print_macro(IPPROTO_RSVP );  /* RSVP protocol   */
    print_macro(IPPROTO_GRE );  /* Cisco GRE tunnels (rfc 1701,1702) */
    print_macro(IPPROTO_IPV6  );  /* IPv6-in-IPv4 tunnelling  */
    print_macro(IPPROTO_ESP );            /* Encapsulation Security Payload protocol */
    print_macro(IPPROTO_AH );             /* Authentication Header protocol       */
    print_macro(IPPROTO_BEETPH );        /* IP option pseudo header for BEET */
    print_macro(IPPROTO_PIM    );  /* Protocol Independent Multicast */
    print_macro(IPPROTO_COMP   );                /* Compression Header protocol */
    print_macro(IPPROTO_SCTP   );  /* Stream Control Transport Protocol */
    print_macro(IPPROTO_UDPLITE ); /* UDP-Lite (RFC 3828)   */
    print_macro(IPPROTO_RAW  );  /* Raw IP packets   */


    return 0;
}

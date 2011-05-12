/*
 * UDPclient.c -- a UDP socket client
 *
 * Invocation:
 *   >./UDPclient
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <string.h> // for memset()
#include <unistd.h> // for close()
#define MYPORT 7188		/* the port to which the server is bound */

int main(int argc, char *argv[])
{
  int sfd;			/* the socket for communication */
  struct sockaddr_in sv_addr, m_addr;	/* s(erver) and m(y) addr data */
  //char buf[1024];
  int n;

  memset(&m_addr, 0, sizeof(m_addr));	/* my address information */
  m_addr.sin_family = AF_INET;
  m_addr.sin_port = 0;			/* 0 ==> assign me a port */
  m_addr.sin_addr.s_addr = htonl(INADDR_ANY);

  memset(&sv_addr, 0, sizeof(sv_addr));	/* server addr info */
  sv_addr.sin_family = AF_INET;
  sv_addr.sin_port = htons(MYPORT);
  sv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

/**** open the UDP socket */
  if ((sfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket");
    return(-1);
  }

/**** bind to local UDP port (randomly assigned) */
  if (bind(sfd, (struct sockaddr *)&m_addr, sizeof(m_addr)) < 0) {
    perror("bind");
    return(-1);
  } else {
  printf("address %d,port %d bound\n",m_addr.sin_addr.s_addr,m_addr.sin_port);
}
/**** send each line from stdin as a separate message to server */
//  while (fgets(buf, sizeof(buf), stdin) != NULL) {
char buf[]="Hello, world!\n";
    n = strlen(buf) + 1;	/* include the EOS! */
    int res =sendto(sfd, buf, n, 0, (struct sockaddr *)&sv_addr, sizeof(sv_addr));
    printf("retval = %d\n",res);
 // }

/**** close the socket */
  close(sfd);
    return 0;
}

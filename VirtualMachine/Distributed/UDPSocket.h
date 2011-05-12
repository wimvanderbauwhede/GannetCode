/*
 * UDPSocket, interface very close to Ruby's UDPSocket
 *
 */

#ifndef __UDPSOCKET_H__
#define __UDPSOCKET_H__

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h> // for memset()
#include <unistd.h> // for close()
#if VERBOSE==1 
#include <iostream>
#endif
#include "../SBA/Types.h"


//#define GWPORT 7188

namespace SBA {

class UDPSocket {
    public:
        int bind(unsigned int ip_addr, unsigned int port);
        int bind(char* ipstr, unsigned int port);
        char* recvfrom(int nbytes);
        int send(Word_List buf,int flag, unsigned int hostaddr, unsigned int port);
        int send(Word_List buf,int flag, char* ipstr, unsigned int port);
        int send(void* buf,int len, unsigned int hostaddr, unsigned int port);
        int send(void* buf,int len, char* ipstr, unsigned int port);
        void close();       
        unsigned int nbytes;
        
    private:        
      int sfd;			/* the socket for communication */
      struct sockaddr_in n_addr;	/* node, src and dst addr data */
      struct sockaddr_in dst_addr;
      struct sockaddr_in src_addr;
      
//      char rx_buffer[MAX_PACKET_SZ*NBYTES];
      int status;
    public:
    UDPSocket() : nbytes(0), status(0) {
        memset(&n_addr, 0, sizeof(struct sockaddr_in));	/* node address information */
        std::cout << "assign\n";
        n_addr.sin_family = AF_INET;
        n_addr.sin_port = 0;			/* 0 ==> assign me a port */
        n_addr.sin_addr.s_addr = htonl(INADDR_ANY);
        memset(&src_addr, 0, sizeof(struct sockaddr_in));	/* src address information */
        src_addr.sin_family = AF_INET;
        src_addr.sin_port = htons(GWPORT);;			/* 0 ==> assign me a port */
        src_addr.sin_addr.s_addr = htonl(INADDR_ANY);

        memset(&dst_addr, 0, sizeof(struct sockaddr_in));	/* dst addr info */
        dst_addr.sin_family = AF_INET;
        dst_addr.sin_port = htons(GWPORT);
        dst_addr.sin_addr.s_addr = htonl(INADDR_ANY); // inet_addr("127.0.0.1");

        /**** open the UDP socket */
        if ((sfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
            status=-1;
#if VERBOSE==1            
            std::cout << "Socket call failed!\n";
#endif            
        } else {
#if VERBOSE==1        
            std::cout << "Socket created!\n";
#endif            
        }
    };
    ~UDPSocket() {
        close();
    };
      
};

}

#endif // __UDPSOCKET_H__

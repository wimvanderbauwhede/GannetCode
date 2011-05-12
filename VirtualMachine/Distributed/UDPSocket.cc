/*
 * UDPSocket, interface very close to Ruby's UDPSocket
 *
 */
#include "UDPSocket.h"
#if VERBOSE==1 
#include <iostream>
#endif
//using namespace SBA;
/**** bind to local UDP port */
int SBA::UDPSocket::bind(unsigned int hostaddr, unsigned int port) {
    if (port!=0) {
    n_addr.sin_port = htons(port);
    }
    if (hostaddr!=INADDR_ANY) {
    n_addr.sin_addr.s_addr = htonl((u_int32_t)hostaddr);
    }
    if (::bind(sfd, (struct sockaddr *)&n_addr, sizeof(struct sockaddr_in)) < 0) {
#if VERBOSE==1     
        std::cout << "bind failed!\n";
#endif        
        return(-1);
    } else {
#if VERBOSE==1     
    std::cout << "address "<<n_addr.sin_addr.s_addr<<", port "<<n_addr.sin_port<<" bound!\n";
#endif    
        return(0);
    }
}

int SBA::UDPSocket::bind(char* ip_str, unsigned int port) {
    unsigned int hostaddr= ::inet_addr(ip_str);
    return SBA::UDPSocket::bind(ntohl(hostaddr), port) ;
    
}

/**** send txbuffer to server */
int SBA::UDPSocket::send(SBA::Word_List txbuffer, int flag, unsigned int hostaddr, unsigned int port) {      
    dst_addr.sin_port = htons(port); // cast to short?
    dst_addr.sin_addr.s_addr = htonl((u_int32_t)hostaddr);
    char* txcharbuffer=txbuffer.to_charbuf();
    nbytes=txbuffer.nbytes();
    int res=sendto(sfd, txcharbuffer, nbytes, flag, (struct sockaddr *)&dst_addr, sizeof(struct sockaddr_in));
    return res;
}

int SBA::UDPSocket::send(Word_List txbuffer,int flag,char* ip_str , unsigned int port) {  
    unsigned int hostaddr= ::inet_addr(ip_str);
    return SBA::UDPSocket::send(txbuffer,flag,  ntohl(hostaddr),  port);
}

int SBA::UDPSocket::send(void* txbuffer,int nbytes, unsigned int hostaddr, unsigned int port) {      
    dst_addr.sin_port = htons(port); // cast to short?
    dst_addr.sin_addr.s_addr = htonl((u_int32_t)hostaddr);
#if VERBOSE==1     
    std::cout << "SBA::UDPSocket::send(): Sending <"<<(char*)txbuffer<<"> ("<<nbytes<<" bytes) to address "<<  dst_addr.sin_addr.s_addr<<", port "<< dst_addr.sin_port <<"\n";
#endif    
    int res= ::sendto(sfd, txbuffer, nbytes, 0, (struct sockaddr *)&dst_addr, sizeof(struct sockaddr_in));
    //std::cout << "retval = "<<res<<"\n";
    return res;
}
int SBA::UDPSocket::send(void* txbuffer,int nbytes,char* ip_str , unsigned int port) {  
    unsigned int hostaddr= ::inet_addr(ip_str);
    return SBA::UDPSocket::send(txbuffer, nbytes,  ntohl(hostaddr), port);
}

/**** receive each message on the socket, printing on stdout */
char* SBA::UDPSocket::recvfrom(int nbytes) {
    int n=0;
    char* rx_buffer=(char*)malloc(nbytes);
#if VERBOSE==1     
    std::cout << "Ready to recv "<<nbytes<< " bytes\n";
#endif    
    while (n==0) {
        memset(&src_addr, 0, sizeof(struct sockaddr_in));
        socklen_t len = (socklen_t)sizeof (struct sockaddr_in);     
#if VERBOSE==1         
        std::cout <<"SBA::UDPSocket::recvfrom(): Waiting for packet ...\n";
#endif        
        n = ::recvfrom(sfd, (void*)rx_buffer, nbytes, 0, (struct sockaddr *)&src_addr, &len);
    }    
    if (n < 0) {
#if VERBOSE==1     
      std::cout << "SBA::UDPSocket::recvfrom(): error!\n";
#endif      
      return(NULL);
    }
#if VERBOSE==1     
    std::cout << "... got packet!\n";
#endif    
    return(rx_buffer);
}

void SBA::UDPSocket::close() {
    ::close(sfd);
}



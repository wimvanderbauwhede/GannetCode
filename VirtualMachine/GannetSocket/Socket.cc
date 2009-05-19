#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <arpa/inet.h>   
#include <signal.h> 
#ifdef VERBOSE
#include <iostream>
#include <string>
#endif //VERBOSE
#include "Socket.h"

using namespace std;
using namespace GannetSocket;

/**
 * Function: default constructor
 */
Socket::Socket() {
}

/**
 * Function: constructor
 * Input: servicePort: the port of the socket service
 */
Socket::Socket(int servicePort) {
    sock_fd = -1;
    port = servicePort;
}

/**
 * Function: constructor
 * Input: servicePort: the port of the socket service
 *            serciceIp: the socket server IP address
 */
Socket::Socket(int servicePort, string serverIp) {
    sock_fd = -1;
    port = servicePort;
    host = serverIp;
}

/**
 * Function: destructor
 */
Socket::~Socket() {
    if (sock_fd > 0)
        close(sock_fd);
    sock_fd = -1;
}

/**
 * Function: create IPv4 stream socket
 * Return: boolean value
 */
bool Socket::createSocket() {
    sock_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (sock_fd < 0)
        return false;
        
    //specify timeout for send and recv function
    setSendRecvTimeout(sock_fd, 30, 30);
    return true;
}

/**
 * Function: create Ipv4 stream socket
 * Output: *fd: the address pointer of the socket operation descriptor
 * Return: boolean value
 */
bool Socket::createSocket(int *fd) {
    //create TCP stream socket
    *fd = socket(AF_INET, SOCK_STREAM, 0);
    if (*fd < 0)
        return false;
        
    //specify timeout for send and recv function
    setSendRecvTimeout(*fd, 30, 30);
    return true;
}

/**
 * Function: set timeout value for send and recv function
 * Input: sTime: the timeout value for send function
 *            rTime: the timeout vaule for recv function
 */
void Socket::setSendRecvTimeout(int fd, int sTime, int rTime){
    struct timeval timeout;
    
    //send timeout
    if (sTime != 0)
    {
        timeout.tv_sec = sTime;   //seconds
        timeout.tv_usec = 0;    //microseconds
        setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, (const void *)&timeout, sizeof(timeout));
    }
    //receive timeout
    if (rTime != 0)
    {
        timeout.tv_sec = rTime;   //seconds
        timeout.tv_usec = 0;    //microseconds
        setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, (const void *)&timeout, sizeof(timeout));
    }
}

/**
 * Function: socket server bind
 * Input: fd: the socket descriptor of the server
 * Return: boolean value
 */
bool Socket::serverBind(int fd){
    
    struct sockaddr_in my_addr;
    
    memset(&my_addr, 0x00, sizeof(my_addr));
    
    my_addr.sin_family = AF_INET;
    my_addr.sin_addr.s_addr = INADDR_ANY;
    my_addr.sin_port = htons ( port );

    if((bind(fd, (struct sockaddr *)&my_addr, sizeof(my_addr))) < 0) {
        perror("bind");
        return false;
    }
    else
        return true;
}

/**
 * Function: socket server listen service
 * Input: fd: the socket descriptor of the server
 * Return: boolean value
 */
bool Socket::serverListen(int fd){
    if ((listen(fd, SOMAXCONN)) < 0)
        return false;
    else
        return true;
}

/**
 * Function: abort the socket connection immediately when it is closed
 * Input: fd: the socket descriptor of the server
 * Return: boolean value
 */
bool Socket::setSocketOption(int fd)
{
    int optlen;
    int on=1;
    struct linger LingerVar;
    
    LingerVar.l_onoff = 1;
    LingerVar.l_linger = 0;
    optlen = sizeof (LingerVar);
    if (setsockopt (fd, SOL_SOCKET, SO_LINGER,
                    (const char *)&LingerVar, optlen) !=0)
    {
        return false;
    }

    if (setsockopt (fd, SOL_SOCKET, SO_OOBINLINE,
                        (char *)&on, sizeof (on)) !=0)
    {
        return false;
    }

    return true;
}

/**
 * Function: client connects to server
 * Input: fd: the socket descriptor of the client
 * Return: boolean value
 */
bool Socket::clientConnect(int fd){
    struct sockaddr_in my_addr;
    struct timeval tv;
    fd_set fds;
    
    memset(&my_addr, 0x00, sizeof(my_addr));
    my_addr.sin_family = AF_INET;
    my_addr.sin_port = htons ( port );

    if ((inet_pton(AF_INET, host.c_str(), &my_addr.sin_addr)) <= 0)
    {
#ifdef VERBOSE
        cout << "inet_pton function error!" <<endl;
#endif //VERBOSE
        return false;
    }

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    //wait up to 10 seconds
    tv.tv_sec = 10;
    tv.tv_usec = 0;
    
    //retval = select(sock_fd + 1, &fds, NULL, NULL, &tv);
    //if (retval == -1)
    //{
#ifdef VERBOSE
        //cout << "select() error!\n";
#endif //VERBOSE
    //    return false;
    //}
    //else if (retval == 0) 
    //{
#ifdef VERBOSE
        //cout << "No data within ten seconds.\n";
#endif //VERBOSE
    //    return false;
    //}
    //else
    //{
#ifdef VERBOSE
        cout << "Data is available now.\n";
#endif //VERBOSE
    //}

    if ((connect(fd, (sockaddr *)&my_addr, sizeof(my_addr))) != 0)
        return false;

    setSocketOption(fd);
    
    return true;
}

/**
 * Function: server accept the connection requirement of the client
 * Input: fd: the listen socket descriptor of the server
 * Output: sock_accept: the address pointer of the accepted socket descriptor
 * Return: boolean value
 */
bool Socket::serverAccept(int fd, int *sock_accept) {
    
    struct sockaddr_in cliaddr;
    socklen_t len = sizeof(cliaddr);
    
    memset(&cliaddr, 0x00, sizeof(cliaddr));
    
    *sock_accept = accept(fd, (sockaddr *)&cliaddr, &len);
    if(*sock_accept < 0)
    {
        perror("accept");
        return false;
    }
    else
        return true;
}

/**
 * Function: send a unsigned int value to socket
 * Input: fd: the socket descriptor
 *            num: the int value to be sent
 * Return: boolean value
 */
bool Socket::sendInt(int fd, uint32_t num) {

    uint32_t tmp = htonl(num);

#ifdef VERBOSE
     cout << "send len after convert: " << tmp << endl;
#endif //VERBOSE
   
    if (send(fd, (char *)&tmp, sizeof(uint32_t), 0) != sizeof(uint32_t)) {
#ifdef VERBOSE
        perror("sendInt");
#endif //VERBOSE
        return false;    
    }
    
    return true;
}

/**
 * Function: receive a unsigned int value from socket
 * Input: fd: the socket descriptor
 * Output: num: the address pointer point to the received int value
 * Return: boolean value
 */
bool Socket::recvInt(int fd, uint32_t *num) {

    uint32_t tmp = 0;
    
    if (recv(fd, (char *)&tmp, sizeof(uint32_t), 0) != sizeof(uint32_t)) {
#ifdef VERBOSE
        perror("recvInt");
#endif //VERBOSE
        return false;    
    }
    
#ifdef VERBOSE
     cout << "recv len before convert: " << dec << tmp << endl;
#endif //VERBOSE
           
    *num = ntohl(tmp);
    
    return true;
}

/**
 * Function: send data to socket, the length of data is specified
 * Input: fd: the socket descriptor
 *            data: the address pointer point to the data memory
 *            len: the length of the data
 * Return: the length of the data which has been sent
 */
int Socket::sendData(int fd, const char *data, uint len) {
    
    uint offset = 0;
    int sendLen = 0;
    
    //send data to peer
    while(offset != len)
    {
        if((sendLen = send(fd, data + offset, len - offset, 0)) < 0) {
#ifdef VERBOSE
            perror("send data");
#endif //VERBOSE
            return -1;
        }
        offset += sendLen;
    }    
    
#ifdef VERBOSE
    cout << "send len : " << offset << endl;
#endif //VERBOSE
        
    return offset;
}

/**
 * Function: receive data from socket, the length of data is specified
 * Input: fd: the socket descriptor
 *            len: the length of the data
 * Output: data: the address pointer point to the data memory
 * Return: the length of the received data
 */
int Socket::recvData(int fd, char *data, uint len) {
    
    uint offset = 0;
    int recvLen = 0;

    //receive data from peer
    while(offset != len)
    {
        if((recvLen = recv(fd, data + offset, len - offset, 0)) < 0) {
#ifdef VERBOSE
            perror("recv data");
#endif //VERBOSE
            return -1;
        }
        offset += recvLen;
    }

#ifdef VERBOSE
    cout << "recv len : " << recvLen << endl;
#endif //VERBOSE
    
    return recvLen;
}

/**
 * Function: close socket description
 * Input: sfd: the socket descriptor
 */
void Socket::closeSocket(int sfd) {
    if (sfd > 0)
        close(sfd);
    sfd = -1;
}

/**
 * Function: obtain the local ethernet interface IP address
 * Input: ethNum: the number of the ethernet interface
 * Return: the ethereal IP address
 */
string Socket::getLocalAddress(int ethNum) {
    
    int raw_sock;
    struct ifreq ifr;
    struct sockaddr_in *addr;
    
    /* create a channel to the NET kernel */
    raw_sock = socket(PF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (raw_sock < 0) {
#ifdef VERBOSE
        cout << "get_myaddr: socket error!" << endl;
#endif
        return "";
    }

    sprintf(ifr.ifr_name, "eth%d", ethNum);
    if (ioctl(raw_sock, SIOCGIFADDR, &ifr) < 0) {
#ifdef VERBOSE
        cout << "get_myaddr::ioctl:" << endl;
#endif
        return "";
    }
    
    addr = (struct sockaddr_in *)&ifr.ifr_addr;
    string ip = inet_ntoa(addr->sin_addr);
#ifdef VERBOSE
    cout << ifr.ifr_name << " " << ip << endl;
#endif
    
    return ip;
        
}

void do_signal(int sig) {
#ifdef VERBOSE
    cout << "capture signal: " << sig << endl;
#endif //VERBOSE
    signal(sig, do_signal);
}

/**
 * Function: create daemon process for socket service of the server
 */
void Socket::initDaemon() {
    
    if(fork() != 0)
        exit(0);
        
    signal(SIGHUP, SIG_IGN);
    
    if(fork() != 0)
        exit(0);

    umask(0);
    
    for (int i = 3; i < 64; i ++)
        close(i);
}

/**
 * Function: create socket service for server
 * Output: fd: the address pointer point to socket descriptor
 * Return: the boolean value
 */
bool Socket::createServer(int *fd) {
    
    int sockfd;
    
    //set server as daemon process
//#ifndef VERBOSE
    initDaemon();
//#endif //VERBOSE
    
    for(int i = 1; i < NSIG; i ++)
        signal(i, SIG_IGN);
        
    //create server socket
    if (!createSocket(&sockfd))
        return false;
        
    //bind server
    if (!serverBind(sockfd))
        return false;
        
    //server start to listen
    if (!serverListen(sockfd))
        return false;
        
    signal(SIGCHLD, SIG_IGN);
    //for (int j = 0; j < NSIG; j++) {
    //    signal(j, do_signal);
    //}

    *fd = sockfd;
    
    return true;
}

/**
 * Function: create socket service for client
 * Return: the boolean value
 */
bool Socket::createClient() {
    
    //create client socket
    if (!createSocket())
        return false;
    
    //connect to server
    if (!clientConnect(sock_fd))
        return false;
        
    return true;
}

/**
 * Function: create socket service for client
 * Output: fd: the address pointer point to socket descriptor
 * Return: the boolean value
 */
bool Socket::createClient(int *fd) {
    
    int sockfd;
    
    //create client socket
    if (!createSocket(&sockfd))
        return false;
    
    //connect to server
    if (!clientConnect(sockfd))
        return false;
        
    *fd = sockfd;
    
    return true;
}


#ifndef _MYSOCKET_H_
#define _MYSOCKET_H_

#include <string.h>
#include <stdlib.h>

#include <string>
using namespace std;

namespace GannetSocket {
    
typedef unsigned char   UCHAR; 
typedef unsigned short  USHORT;

#define MY_DEFAULT_PORT     6969
#define MY_MAX_CONNECTIONS  50
#define MY_MAX_BUFSIZE      128
#define MY_FILE_NO          64
#define MY_BLOCK_SIZE          64


class Socket {
    public:
        //constructor
        Socket();
        Socket(int servicePort);
        Socket(int servicePort, string serverIp);
        ~Socket();
        bool serverAccept(int fd, int *sock_accept);
        bool createServer(int *fd);
        bool createClient(int *fd);
        bool createClient();
        bool sendInt(int fd, uint32_t num);
        int  sendData(int fd, const char *data, unsigned int len);
        bool recvInt(int fd, uint32_t *num);
        int  recvData(int fd, char *data, unsigned int len);
        void closeSocket(int sfd);
        void serverClose();
        string getLocalAddress(int ethNum);
        void setHostIp(string ip) {host = ip;}
        void setServicePort(int p) {port = p;}
    
    private:
        int sock_fd;
        int port;
        string host;
        
        bool createSocket();
        bool createSocket(int *fd);
        void setSendRecvTimeout(int fd, int sTime, int rTime);
        bool serverBind(int fd);
        bool serverListen(int fd);
        bool clientConnect(int fd);
        bool setSocketOption(int fd);
        void initDaemon();
};  //end of the Socket class definition
}   //end of the namespace Gannet

#endif //_MYSOCKET_H_

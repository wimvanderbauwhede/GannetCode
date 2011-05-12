#ifndef _MORASOCKET_H_
#define _MORASOCKET_H_

#include <string.h>
#include <stdlib.h>

#include <string>
using namespace std;

namespace Mora {
    
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
        bool serverAccept( int *sock_accept);
        bool createServer();
        bool createHostInterface();
        bool sendUInt32(uint32_t num);
        int  sendData(const char *data, unsigned int len);
        bool recvUInt32(uint32_t *num);
        int  recvData(char *data, unsigned int len);
        void closeSocket();
        string getLocalAddress(int ethNum);
        void setHostIp(string ip) {hostaddr = ip;}
        void setServicePort(int p) {port = p;}
        string getHostIp();
        int getServicePort();
    
    private:
        int sock_fd;
        int port;
        string hostaddr;
        
        bool createSocket();
        void setSendRecvTimeout(int sTime, int rTime);
        bool serverBind();
        bool serverListen();
        bool clientConnect();
        bool setSocketOption();
        void initDaemon();
};  //end of the Socket class definition
}   //end of the namespace Mora

#endif //_MORASOCKET_H_

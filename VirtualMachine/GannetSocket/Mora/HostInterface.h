#ifndef __MORASOCKETCLIENT_H__
#define __MORASOCKETCLIENT_H__

#include <string>

#include "Socket.h"
#include "DataTransfer.h"

using namespace std;
using namespace Mora;

#ifndef NBYTES
#define NBYTES 4
#endif

namespace Mora {

class HostInterface {
    public:
        //constructor
        HostInterface();
        HostInterface(string ip, int port);
        //destructor
        ~HostInterface() {c_sock.closeSocket();};
        
        //methods
        //bool createSocket(string ip, int port);
        bool sendBuf(void* buf,int len);
        void recvBuf(void*buf);
        
    private:
        //variables
        bool createSocket();
        string serverIp;
        string serverPort;
        Socket c_sock;
        
};  //end of HostInterface class definition
}   //end of namespace Mora

#endif //__MORASOCKETCLIENT_H__


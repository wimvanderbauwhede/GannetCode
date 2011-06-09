#ifndef __DATATRANSFER_H__
#define __DATATRANSFER_H__

#include "Socket.h"

using namespace std;

namespace GannetSocket {

class DataTransfer {

    public:
        //constructer
        DataTransfer(Socket sock, int fd);
        
        //methods
        bool    sendCodeLength(uint32_t len);
        bool    sendCodeData(const char *data, uint32_t len); //FIXME: void* ?
        int32_t recvCodeLength();
        int32_t recvCodeData(char *buf, uint32_t len); // FIXME: void* ?
        
    private:
        //variables
        Socket mysocket;
        int sockfd;
       
};  //end of DataTransfer class definition
}   //end of namespace MyGannet

#endif //__DATATRANSFER_H__


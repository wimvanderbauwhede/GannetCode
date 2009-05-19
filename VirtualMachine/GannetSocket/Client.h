#ifndef __GANNETSOCKETCLIENT_H__
#define __GANNETSOCKETCLIENT_H__

#include <string>

#include "Socket.h"
#include "DataTransfer.h"
#include "Types.h"

using namespace std;
using namespace SBA;

#ifndef NBYTES
#define NBYTES 4
#endif

namespace GannetSocket {

class Client {
    public:
        //constructor
        Client();
        Client(string ip, string port, string tdc_file);
        //destructor
        ~Client() {client.closeSocket(sockfd);};
        
        //methods
        bool createSocket(string ip, int port);
        bool readCode(void);
        void dumpResult(Word_List result);
        
    private:
        //variables
        string codeFile;
        string serverIp;
        string serverPort;
        Socket client;
        int sockfd;
        
        //methods
        int32_t getFileSize(void);
        List<int32_t> to_signed_int_list( Word_List wl);
        void recvResult(DataTransfer trans);
        void recvResult(DataTransfer *trans);
        
};  //end of Client class definition
}   //end of namespace GannetSocket

#endif //__GANNETSOCKETCLIENT_H__


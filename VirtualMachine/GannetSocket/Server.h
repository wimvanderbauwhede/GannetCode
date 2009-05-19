#ifndef __SERVER_H__
#define __SERVER_H__

//#include <string>
//#include <iostream>
//#include <queue> // WV27082008: TODO: replace with static alternative!

#include "../SBA/Types.h"
#include "Socket.h"
#include "DataTransfer.h"

using namespace std;
using namespace SBA;

namespace GannetSocket {

//typedef queue<Word> QueueWordCode;

class Server {
    public:
        //default constructor
        Server();
        //constructor
        Server(int port);
        //destructor
        ~Server();
        
        //methods
        Bytecode run(uint status);
        void sendResultToClient(Word_List words_result);
        
    private:
        //variables
        int servicePort;
        Socket server;
        int sockfd;
        int accepted_sockfd;
        fd_set master; // master set of file descriptors  
        fd_set read_fds; //set of file descriptors to read from 
        int maxfd; // highest fd in the set 
        int listened_fd;
        int accepted_fd; 
        
        //methods
        Bytecode doWithClientRequest(int fd);
        
};  //end of MyGannetServer class definition
}   //end of namespace MyGannet

#endif //__SERVER_H__


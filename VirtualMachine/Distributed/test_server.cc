#include <iostream>
#define WORDSZ 64

#include "UDPSocket.h"
using namespace SBA;
int main () {

    UDPSocket rx_socket;
    const int port=7188;
    //char ipstr[]="127.0.0.1";
    rx_socket.bind(INADDR_ANY, port);
    char* buf=rx_socket.recvfrom(MAX_PACKET_SZ*NBYTES);
    std::cout << "got buf!\n";    
    std::cout<< buf<< "\n";
    rx_socket.close();
    return 0;

}

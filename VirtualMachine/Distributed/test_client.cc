#define WORDSZ 64
#include <iostream>
#include "UDPSocket.h"
using namespace SBA;
int main () {

UDPSocket tx_socket;

char buf[]="Hello, world!\n";

char ipstr[] ="127.0.0.1";
const int port=7188;
tx_socket.bind(INADDR_ANY,0);
std::cout << "Sending <"<<buf<<"> ...\n";
int res=tx_socket.send((void*)buf, strlen(buf)+1,ipstr, port);
std::cout << "retval = "<<res<<"\n";
tx_socket.close();
return res;

}

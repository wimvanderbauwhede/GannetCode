#include <string>
#include <iostream>

#include "Server.h"

using namespace std;
using namespace GannetSocket;


int main(int argc, char *argv[])
{
    string port;

    if( argc == 1) {
//        cout << "Usage: gserver SERVICE_PORT" << endl;
        port = "6969";
    }
    else
        port = argv[1];

#ifdef DUMP
    cout << "Server begin.....\n";
#endif //DUMP

    //read code file and send it to Gannet server
    Server server(port);
    server.startService();

#ifdef DUMP
    cout << "Server end.....\n";
#endif //DUMP

    return 0;
}


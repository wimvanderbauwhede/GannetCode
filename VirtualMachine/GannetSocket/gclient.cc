#include <string>
#include <iostream>

#include "Client.h"

using namespace std;
using namespace GannetSocket;


int main(int argc, char *argv[])
{
    
    if( argc < 4) {
        cout << "Usage: gclient SERVER_IP PORT FILE_NAME" << endl;
        return 0;
    }
        
#ifdef DUMP
    cout << "Client begin.....\n";
#endif //DUMP

    //read code file and send it to Gannet server
    Client client(argv[1], argv[2], argv[3]);
    client.readCode();

#ifdef DUMP
    cout << "Client end.....\n";
#endif //DUMP
   
    return 0;
}


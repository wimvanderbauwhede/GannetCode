
#include <sys/time.h>
#include <sys/select.h>

#ifdef VERBOSE
#include <string>
#include <iostream>
#endif // VERBOSE
#include "Server.h"
#include "../SBA/Runtime.h"

using namespace std;
using namespace SBA;
using namespace GannetSocket;

/**
 * Function: the default constructor
 */
Server::Server() {

    servicePort = MY_DEFAULT_PORT;
    FD_ZERO(&read_fds);
    FD_ZERO(&master);
    // get the current size of file descriptors table
    maxfd = getdtablesize();

#ifdef VERBOSE
//    cout << "Server begin.....\n";
//    cout << "Server port: " << servicePort << endl;
#endif //VERBOSE

    //create socket server
    server.setServicePort(servicePort);
    server.createServer(&listened_fd);

    // add listened_fd to the master set
    FD_SET(listened_fd, &master);

    maxfd = listened_fd + 1;
}

/**
 * Function: the constructor
 * Input: port: the service port of the socket
 */
Server::Server(int port) {

    servicePort = port;
}

/**
 * Function: the destructor
 */
Server::~Server() {

    close(accepted_sockfd);
    server.closeSocket(sockfd);
}

/**
 * Function: server receives the byte codes from the client and push the
 *                 codes to the word deque
 * Input: fd: the accepted socket descriptor
 * Return: the word deque
 */
Bytecode Server::doWithClientRequest(int fd){

    string fileName;
    uint32_t codeLen;
    int recvLen;
    Bytecode code_deque;

#ifdef VERBOSE
    cout << "\n****** Method: doWithClientRequest ******\n" << endl;
#endif //VERBOSE

    //create object of Class DataTransfer for sending data to server
    DataTransfer trans(server, fd);

    //receive file length from client
    if ((codeLen = trans.recvCodeLength()) < 0) {
        return code_deque;
    }

#ifdef VERBOSE
    cout << "Received code length: " << dec << codeLen << endl;
#endif //VERBOSE

    //calculate the block num to read
    uint blockNum = (codeLen-1)/MY_BLOCK_SIZE + 1;

     //receive data from client
    for (uint i = 0; i < blockNum; i ++) {

        uint32_t len;
        char buf[MY_MAX_BUFSIZE];
        memset(buf, 0x00, sizeof(buf));

        if(i == blockNum -1)
            len = codeLen - (MY_BLOCK_SIZE * i);
        else
            len = MY_BLOCK_SIZE;

        //receive data from client
        if ((recvLen = trans.recvCodeData(buf, len)) < 0) {
            break;
        }

#ifdef VERBOSE
        cout << "Received code data (bytes):\n";
        for (int k = 0; k < recvLen; k++)
            cout << hex << (int)buf[k] << " ";
        cout <<endl;
#endif //VERBOSE
#ifdef VERBOSE
        cout << "Received code data (Words):\n";
#endif

        for (int j = 0; j < recvLen/4; j ++) {
            Word w = 0;
            for (int k = 0; k < NBYTES; k++) {
                Word byte = 0;
                byte = (Word)(buf[j*4 + k] & 0xff);
                w += (byte << (8 * (NBYTES - 1 - k)));
            }
/*
	for (int j = 0; j < recvLen; j ++) {
            Word w = 0;
            for (int k = 0; k < NBYTES; k++) {
                Word byte = (Word)buf[j];
                w += (byte << (8 * (NBYTES - 1 - k)));
            }
*/
#ifdef VERBOSE
            cout << hex << w;
            cout << "[DEC: " << dec << w << "]" << " ";
#endif //VERBOSE
            code_deque.push_back(w);
#ifdef VERBOSE
            cout << "code_deque["<<j<<"]: " << dec << code_deque.at(j) << "]" << " ";
#endif //VERBOSE
        }
    }
#ifdef VERBOSE
    cout << endl;
#endif //VERBOSE

    return code_deque;
}

/**
 * Function: server sends the running results which were received from
 *                 the Gannet to the client
 * Input: fd: the accepted socket descriptor
 *            words_result: the results described by word deque
 */
void Server::sendResultToClient(Word_List words_result) {

    int fd = accepted_sockfd;

#ifdef VERBOSE
    cout << "Server send result to client!\n";
#endif //VERBOSE

    uint32_t codeLen, len;

    //create object of Class DataTransfer for sending data to server
    DataTransfer trans(server, fd);

    //calculate the byte length of the list
    codeLen = words_result.size() * sizeof(Word);

    //send file length to server
    if(!trans.sendCodeLength(codeLen)) {
#ifdef VERBOSE
            cout << "Server send length error!\n";
#endif //VERBOSE
        return;
    }

    //calculate the block num to send
    uint32 blockNum = (codeLen-1)/MY_BLOCK_SIZE + 1;

    //send the result to client
    for (uint i = 0; i < blockNum; i ++) {

        char buf[MY_BLOCK_SIZE];
        memset(buf, 0x00, sizeof(buf));

        if(i == blockNum -1)
            len = codeLen - (MY_BLOCK_SIZE * i);
        else
            len = MY_BLOCK_SIZE;

        for (uint j = 0; j < len/4; j ++) {

            Word w = words_result.front();
            for (int k = 0; k < NBYTES; k++) {
                buf[j*4 + k] = (w >> (8 * (NBYTES - 1 - k))) & 0xff;
            }
#ifdef VERBOSE
            cout << hex << w << endl;
#endif //VERBOSE
            words_result.pop_front();
        }

        //communicate with client
        if(!trans.sendCodeData(buf, len)) {
#ifdef VERBOSE
            cout << "Server send data error!\n";
#endif //VERBOSE
            break;
        }
    }
    close(fd);
}

/**
 * Function: start server. listen, accept, and treats with the connection request from the client
 */
Bytecode Server::run(uint status){

    Bytecode bytecodes;
    struct timeval tv;

//listen and accept the connection request from the client

    tv.tv_sec = 0;
    tv.tv_usec = 0;
        read_fds = master;
        // never time out
#ifdef VERBOSE
        cout << "begin select... "  << endl;
#endif //VERBOSE
    if(status == 0) //CS_idle
        select(maxfd, &read_fds, NULL, NULL, (struct timeval *)NULL);
    else
        select(maxfd, &read_fds, NULL, NULL, &tv);
#ifdef VERBOSE
        cout << "select ok! "  << endl;
#endif //VERBOSE
        //run through the existing connections looking for data to read
        for(int i = 0; i < maxfd; i++) {
            if (FD_ISSET(i, &read_fds)) { // if i belongs to the set read_fds
                if (i == listened_fd) { // fd of server socket
                    // accept on new client socket newfd
                    server.serverAccept(listened_fd, &accepted_fd);
#ifdef VERBOSE
                    cout << "Server accepted connection: " << accepted_fd << endl;
#endif //VERBOSE
                    accepted_sockfd = accepted_fd;
                    bytecodes = doWithClientRequest(accepted_sockfd);

#ifdef VERBOSE
                    cout << "dowithclient: bytecodes[0]="<<bytecodes.at(0) << endl;
                    cout << "dowithclient: bytecodes[1]="<<bytecodes.at(1) << endl;
#endif //VERBOSE
                }
            } // FD_ISSET
        } // for i


    return bytecodes;
}


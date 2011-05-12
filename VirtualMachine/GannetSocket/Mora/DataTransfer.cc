#ifdef VERBOSE
#include <string>
#include <iostream>
#endif // VERBOSE
#include "DataTransfer.h"

using namespace std;
using namespace GannetSocket;

/**
 * Function: the constructor
 * Input: sock: the object of the MySocket class
 *            fd: the socket descriptor
 */
DataTransfer::DataTransfer(const Socket& sock) : mysocket(sock) {

}

/**
 * Function: send the length of the data wanted to be sent later
 * Input: len: the length value
 * Return: the boolean value
 */
bool DataTransfer::sendCodeLength(uint32_t len) {
    
    if(!mysocket.sendUInt32(len)) {
        return false;
    }
#ifdef VERBOSE
    cout << "Send code length: " << len << endl;
#endif //VERBOSE

    char buf[MY_MAX_BUFSIZE];
    memset(buf, 0x00, sizeof(buf));

    if(mysocket.recvData( buf, 1) < 0) {
#ifdef VERBOSE
        cout << "Client recv data error!\n";
#endif //VERBOSE
        return false;
    }
#ifdef VERBOSE
    cout << "Received result: " << buf[0] << endl;
#endif //VERBOSE
    if(buf[0] != 'T') {
        return false;
    }
    
    return true;
}

/**
 * Function: send the data to socket, the length is specified
 * Input: data: the address pointer point to the data memory
 *            len: the length of the data
 * Return: the boolean value
 */
bool DataTransfer::sendCodeData(const char *data, uint32_t len) {
    
    if(len == 0) {
#ifdef VERBOSE
        cout << "the send length is error!\n";
#endif //VERBOSE
        return false;
    }
    
    if(!mysocket.sendData(sockfd, data, len)) {
#ifdef VERBOSE
        cout << "Client send int error!\n";
#endif //VERBOSE
        return false;
    }

    char buf[MY_MAX_BUFSIZE]; // FIXME: calloc
    memset(buf, 0x00, sizeof(buf));

    if(mysocket.recvData( buf, 1) < 0) {
#ifdef VERBOSE
        cout << "Client recv data error!\n";
#endif //VERBOSE
        return false;
    }
#ifdef VERBOSE
    cout << "Received result: " << buf[0] << endl;
#endif //VERBOSE
    if(buf[0] != 'T') {
#ifdef VERBOSE
        cout << "Client got a error from server!\n";
#endif //VERBOSE
        return false;
    }
    
    return true;
}

/**
 * Function: receive the length of the data
 * Return: the length value
 */
int32_t DataTransfer::recvCodeLength() {

    uint32_t len = 0;
    
    //receive file length from client
    if (!mysocket.recvUInt32( &len)) {
#ifdef VERBOSE
        cout << "Receive data error or client has been closed!\n";
#endif //VERBOSE
        return -1;
    }
/*
    if (!mysocket.sendData( "T", 1)) {
#ifdef VERBOSE
        cout << "Send data error or client has been closed!\n";
#endif //VERBOSE
        return -1;
    }
  */  
    return len;
}

/**
 * Function: receive the data, the data length is specified
 * Input: len: the length of the data
 * Output: data: the address pointer point to the data memory
 * Return: the length of the received data
 */
int32_t DataTransfer::recvCodeData(char *buf, uint32_t len) {

    uint32_t recvLen = 0;
    
    if(len == 0) {
#ifdef VERBOSE
        cout << "the receive length is error!\n";
#endif //VERBOSE
        return false;
    }
    
    //receive file data from client
    if ((recvLen = mysocket.recvData( buf, len)) < 0) {
#ifdef VERBOSE
        cout << "Receive data error or client has been closed!\n";
#endif //VERBOSE
        return -1;
    }
    
#ifdef VERBOSE
    cout << "Received length: " << recvLen << endl;
#endif //VERBOSE
/*
    if (!mysocket.sendData(sockfd, "T", 1)) {
#ifdef VERBOSE
        cout << "Send data error or client has been closed!\n";
#endif //VERBOSE
        return -1;
    }
  */  
    return recvLen;
}



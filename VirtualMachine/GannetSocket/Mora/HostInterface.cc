#include <sys/stat.h>

#include <string>
#include <iostream>

#include "HostInterface.h"

using namespace std;
using namespace GannetSocket;

/**
 * Function: the default constructor
 */
HostInterface::HostInterface() {
    createSocket(); // FIXME: Error handling
}
/**
 * Function: the constructor
 * Input: ip: the socket server IP address
 *            port: the socket service port of the server
 *            tdc_file: the code file name
 */
HostInterface::HostInterface(string ip, string port, string tdc_file) {

    serverIp = ip;
    serverPort = port;
    createSocket(); // FIXME: Error handling
}

/**
 * Function: create socket for c_sock
 * Input: ip: the socket server IP address
 *            port: the socket service port of the server
 * Return: the boolean value
 */
bool HostInterface::createSocket() {

    //create socket c_sock
    c_sock.setHostIp(ip);
    c_sock.setServicePort(port);
    if(!c_sock.createHostInterface()) {
#ifdef DUMP
        cout << "Create socket c_sock error!\n";
#endif //DUMP
        return false;
    }
    return true;
}

/**
 * Function: print the the values of the words deque
 * Input: result: the words deque
 */
void HostInterface::dumpResult(Word_List result) {
    
#ifdef DUMP
    cout << "RESULT: ( ";
#endif // DUMP
    List<int> int_result=to_signed_int_list(result);
    bool first=true;
    for(List<int>::iterator iter_=int_result.begin();iter_!=int_result.end();iter_++) {
        int elt=*iter_;
        if ((elt > -0x22FFFFFF and elt < 0xDD000000) or not first){
            cout << dec << elt  << " ";
            first=false;
        }
    }
#ifdef DUMP
    cout << ")";
#endif // DUMP
    cout << "\n";
}

/**
 * Function: receive the running result of the Gannet from server
 * Input: trans: the object of the DataTransfer class
 */
void HostInterface::recvResult(DataTransfer trans) {

    int32_t bufsz, recvLen;
    Word_List words_result; 
    
    //receive file length from c_sock
    if ((bufsz = trans.recvLength()) <= 0) {
        return;
    }
    
#ifdef DUMP
    cout << "Received length: " << dec << bufsz << endl;
#endif //DUMP

    //calculate the block num to read
    uint blockNum = (bufsz-1)/MY_BLOCK_SIZE + 1;

     //receive data from c_sock
    for (uint i = 0; i < blockNum; i ++) {
        
        uint32_t len;
        char buf[MY_BLOCK_SIZE];
        memset(buf, 0x00, sizeof(buf));
        
        if(i == blockNum -1)
            len = bufsz - (MY_BLOCK_SIZE * i);
        else
            len = MY_BLOCK_SIZE;
        
        //receive data from c_sock
        if ((recvLen = trans.recvData(buf, len)) < 0) {
            break;
        }
        
#ifdef DUMP
        cout << "Received data:\n";
        for (int k = 0; k < recvLen; k++) {
            int i = buf[k] & 0xff;
            cout << hex << i << " ";
        }
        cout << endl;
#endif //DUMP
        
        //write data to queue
        for (int j = 0; j < recvLen / 4; j ++) {
            Word w = 0;
            for(uint m = 0; m < NBYTES; m++) {
                Word byte = buf[j * 4 + m] & 0xff;
                w += (byte << (8 * (NBYTES - 1 - m)));
            }
            words_result.push_back(w);
        }

    }
#ifdef DUMP
    cout << endl;
#endif //DUMP

    dumpResult(words_result);
}

/**
 * Function: read the contents from the code file, send the contents to
                    server, and receive the running result from the server
 * Return: the boolean value
 */
bool HostInterface::sendBuf(void* buf, int bufsz) {

    //get the size of the file
    if( bufsz <= 0)
        return false;
    
    //calculate the block num to read
    //WV: not sure if there is any use in chunking the stream, 
    // not for localhost use in any case
    // but maybe make it work for blockNum=1
    int32_t blockNum = (bufsz-1)/MY_BLOCK_SIZE + 1;
    
    //create socket c_sock //FIXME: should happen in constructor!!!
    /*
    if (!createSocket(serverIp, atoi(serverPort.c_str()))) {
#ifdef DUMP
        cout << "Create socket c_sock error!\n";
#endif //DUMP
        return false;
    }
    */
    //send file length to server
    if(!c_sock.sendUInt32(bufsz) {
#ifdef DUMP
            cout << "HostInterface send length error!\n";
#endif //DUMP
        return false;
    }
  
    //read the code file and send it to server
    for (int i = 0; i < blockNum; i ++) {
        
        char bbuf[MY_BLOCK_SIZE];
        memset(bbuf, 0x00, sizeof(buf));
        unsigned int length=MY_BLOCK_SIZE;
        bbuf[i]=buf[j]; // FIXME!
        //communicate with server
        if(!c_sock.sendData(bbuf, length)) {
#ifdef DUMP
            cout << "HostInterface send data error!\n";
#endif //DUMP
            break;
        }
    }    
    
    
    return true;
}


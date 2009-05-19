#include <sys/stat.h>

#include <string>
#include <iostream>

#include "Client.h"

using namespace std;
using namespace GannetSocket;

/**
 * Function: the default constructor
 */
Client::Client() {}

/**
 * Function: the constructor
 * Input: ip: the socket server IP address
 *            port: the socket service port of the server
 *            tdc_file: the code file name
 */
Client::Client(string ip, string port, string tdc_file) {

    serverIp = ip;
    serverPort = port;
    codeFile = tdc_file;
}

/**
 * Function: create socket for client
 * Input: ip: the socket server IP address
 *            port: the socket service port of the server
 * Return: the boolean value
 */
bool Client::createSocket(string ip, int port) {

    //create socket client
    client.setHostIp(ip);
    client.setServicePort(port);
    if(!client.createClient(&sockfd)) {
#ifdef DUMP
        cout << "Create socket client error!\n";
#endif //DUMP
        return false;
    }
    return true;
}

/**
 * Function: get the size of the code file
 * Return: the size of the file
 */
int32_t Client::getFileSize(void) {

    struct stat statBuf;
    if( stat(codeFile.c_str(), &statBuf) < 0) {
#ifdef DUMP
        cout << "Open file error: " << codeFile << endl;
#endif //DUMP
        return -1;
    }    
#ifdef DUMP
    cout << "file size: " << statBuf.st_size << endl;
#endif //DUMP

    return statBuf.st_size;
}

/**
 * Function: convert the deque of the words to the list of the int
 * Input: wl: the deque of the words
 * Return: the list of the int value
 */
List<int32_t> Client::to_signed_int_list( Word_List wl) {
       
    List<int32_t> numlist;
    
    for(Word_List::iterator iter_=wl.begin();iter_!=wl.end();iter_++) {
    	Word w=*iter_;
     int32_t sw=(int32_t)w;
        numlist.push_back(sw);
    }
    
    return numlist;
}     

/**
 * Function: print the the values of the words deque
 * Input: result: the words deque
 */
void Client::dumpResult(Word_List result) {
    
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
void Client::recvResult(DataTransfer trans) {

    int32_t codeLen, recvLen;
    Word_List words_result; 
    
    //receive file length from client
    if ((codeLen = trans.recvCodeLength()) <= 0) {
        return;
    }
    
#ifdef DUMP
    cout << "Received code length: " << dec << codeLen << endl;
#endif //DUMP

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
        
#ifdef DUMP
        cout << "Received code data:\n";
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
bool Client::readCode(void) {

    //get the size of the file
    int32_t fileSize = getFileSize();
    if( fileSize <= 0)
        return false;
    
    //open file for reading
    FILE *fp = fopen(codeFile.c_str(), "rb");
    if(fp == NULL) {
#ifdef DUMP
        cout << "Open file error!\n";
#endif //DUMP
        return false;
    }
    
    //calculate the block num to read
    int32_t blockNum = (fileSize-1)/MY_BLOCK_SIZE + 1;
    
    //create socket client
    if (!createSocket(serverIp, atoi(serverPort.c_str()))) {
#ifdef DUMP
        cout << "Create socket client error!\n";
#endif //DUMP
        fclose(fp);
        return false;
    }
    
    //create object of Class DataTransfer for sending data to server
    DataTransfer trans(client, sockfd);
    
    //send file length to server
    if(!trans.sendCodeLength(fileSize)) {
#ifdef DUMP
            cout << "Client send length error!\n";
#endif //DUMP
        fclose(fp);
        return false;
    }
  
    //read the code file and send it to server
    for (int i = 0; i < blockNum; i ++) {
        
        char buf[MY_BLOCK_SIZE];
        memset(buf, 0x00, sizeof(buf));
        
        //read the code file
        uint length = fread(buf, 1, MY_BLOCK_SIZE, fp);
#ifdef DUMP
        cout << "The length which is read: " << length << endl;
        for (uint j = 0; j < length; j++) 
            //cout << hex << buf[i] << " ";
            printf("%x ", buf[j]);
        cout << endl;
#endif //DUMP

        //communicate with server
        if(!trans.sendCodeData(buf, length)) 
        {
#ifdef DUMP
            cout << "Client send data error!\n";
#endif //DUMP
            break;
        }
    }    
    fclose(fp);
    
    recvResult(trans);
    
    return true;
}


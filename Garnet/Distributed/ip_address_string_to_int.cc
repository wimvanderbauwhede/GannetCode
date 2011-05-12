#include <iostream>
#include <sstream>
#include <string>
using namespace std;
uint32_t ip_str_to_int(string ipstr) {
    const int dot ='.';
    istringstream str2;
    str2.str(ipstr);
    uint32_t bytes[4];
    str2 >> bytes[0];
    str2.ignore(1, dot);
    str2 >> bytes[1]; 
    str2.ignore(1, dot);
    str2 >> bytes[2] ;
    str2.ignore(1, dot);
    str2 >> bytes[3] ;
    for (int i=0;i<4;i++) {
        cout << bytes[i]<< endl;
    }
    uint32_t ipnum = (bytes[0]<<24)+(bytes[1]<<16)+(bytes[2]<<8)+bytes[3];
    cout << ipnum << endl;
    return ipnum;
}

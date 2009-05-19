#ifndef __DEBUG_H__
#define __DEBUG_H__

#include <string>

using namespace std;

namespace GannetSocket {

class Debug {
    public:
        Debug(void);
        Debug(int format);
        Debug(string info);
        Debug(string info1, string info2);
        void displayTitle(string info);
        void displayInfo(string info);
};
} //GannetSocket
#endif //__DEBUG_H__


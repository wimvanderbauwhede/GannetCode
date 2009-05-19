#include <iostream>

#include "Debug.h"

using namespace std;
using namespace GannetSocket;

Debug::Debug(void) {
    cout << hex << endl;
}

Debug::Debug(int format) {
    if (format == 16) {
        cout << hex;
    }
    else if (format == 10) {
        cout << dec;
    }
}

Debug::Debug(string info) {
    cout << "---------------------------------" << endl;
    cout << info << endl;
}

Debug::Debug(string info1, string info2) {
    cout << "---------------------------------" << endl;
    cout << info1 << ": " << info2 << endl;
}

void Debug::displayTitle(string info) {
    cout << "*********** " << info << " ***********" << endl;
    cout << endl;
}

void Debug::displayInfo(string info) {
    cout << info << endl;
}


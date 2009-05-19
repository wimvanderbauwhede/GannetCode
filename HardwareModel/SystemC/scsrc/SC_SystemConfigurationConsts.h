
#ifndef _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_
#define _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_


#include "SC_sba.h"

using namespace std;

typedef unsigned int UINT;

namespace SC_SBA {
const UINT SC_LAMBDA = 0;
const UINT SC_DET1 = 0;
const UINT SC_INV1 = 0;
const UINT SC_GATEWAY = 0;
const UINT SC_DET2 = 0;
const UINT SC_INV2 = 0;
const UINT SC_A = 0;
const UINT SC_ALU = 0;
const UINT SC_SCALE = 0;
const UINT SC_B = 0;
const UINT SC_TRAN = 0;
const UINT SC_CROSS = 0;
const UINT SC_LET = 0;
const UINT SC_MMULT = 0;
const UINT SC_NONE15 = 0;
const UINT SC_IF = 0;
const UINT SC_MADD = 0;

const UINT S_LAMBDA = 16;
const UINT S_DET1 = 5;
const UINT S_INV1 = 11;
const UINT S_GATEWAY = 0;
const UINT S_DET2 = 6;
const UINT S_INV2 = 12;
const UINT S_A = 1;
const UINT S_ALU = 7;
const UINT S_SCALE = 13;
const UINT S_B = 2;
const UINT S_TRAN = 8;
const UINT S_CROSS = 14;
const UINT S_LET = 3;
const UINT S_MMULT = 9;
const UINT S_NONE15 = 15;
const UINT S_IF = 4;
const UINT S_MADD = 10;

const UINT A_READ = 2;
const UINT A_HEAD = 5;
const UINT A_eq = 15;
const UINT A_IFTC = 3;
const UINT A_LIST = 4;
const UINT A_gt = 14;
const UINT A_RETURNTC = 2;
const UINT A_LETTC = 10;
const UINT A_minus = 10;
const UINT A_TAIL = 6;
const UINT A_times = 11;
const UINT A_LENGTH = 7;
const UINT A_CONS = 8;
const UINT A_plus = 9;
const UINT A_lt = 13;
const UINT A_not = 21;
const UINT A_ASSIGN = 1;
const UINT A_APPEND = 9;
const UINT A_over = 12;
const UINT A_RETURN = 1;
const UINT A_UPDATE = 3;

} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/

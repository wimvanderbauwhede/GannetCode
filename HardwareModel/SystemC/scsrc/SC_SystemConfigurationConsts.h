
#ifndef _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_
#define _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_


#include "SC_SBA.h"

using namespace std;

typedef unsigned int UINT;

namespace SC_SBA {
const UINT SC_IMG = 0;
const UINT SC_S4 = 0;
const UINT SC_GATEWAY = 0;
const UINT SC_CONFIG = 0;
const UINT SC_S3 = 0;
const UINT SC_IO = 0;
const UINT SC_ALU = 0;
const UINT SC_S1 = 0;
const UINT SC_BEGIN = 0;
const UINT SC_S5 = 0;
const UINT SC_S2 = 0;
const UINT SC_LET = 0;
const UINT SC_LAMBDA = 0;
const UINT SC_CALL = 0;
const UINT SC_IF = 0;
const UINT SC_PROCIMG = 0;

const UINT S_IMG = 5;
const UINT S_S4 = 11;
const UINT S_GATEWAY = 0;
const UINT S_CONFIG = 6;
const UINT S_S3 = 12;
const UINT S_IO = 1;
const UINT S_ALU = 7;
const UINT S_S1 = 13;
const UINT S_BEGIN = 2;
const UINT S_S5 = 8;
const UINT S_S2 = 14;
const UINT S_LET = 3;
const UINT S_LAMBDA = 9;
const UINT S_CALL = 15;
const UINT S_IF = 4;
const UINT S_PROCIMG = 10;
const UINT S_APPLY = 0;

const UINT A_FCLOSE = 2;
const UINT A_READ = 2;
const UINT A_HEAD = 5;
const UINT A_eq = 15;
const UINT A_IFTC = 3;
const UINT A_LIST = 4;
const UINT A_IMG_SIZE = 1;
const UINT A_s3_return = 31;
const UINT A_gt = 14;
const UINT A_RETURNTC = 2;
const UINT A_s5_run = 1;
const UINT A_IOWRITE = 4;
const UINT A_LETTC = 10;
const UINT A_PROCIMG_DUMP = 1;
const UINT A_s3_confrun = 3;
const UINT A_S_RECONF = 2;
const UINT A_s4_confrun = 3;
const UINT A_s4_reconf = 2;
const UINT A_s3_if = 30;
const UINT A_s1_return = 31;
const UINT A_minus = 10;
const UINT A_FOPEN = 1;
const UINT A_TAIL = 6;
const UINT A_S_CONFRUN = 3;
const UINT A_s1_confrun = 3;
const UINT A_s5_confrun = 3;
const UINT A_s2_if = 30;
const UINT A_times = 11;
const UINT A_IOREAD = 3;
const UINT A_DISPLAY = 5;
const UINT A_LENGTH = 7;
const UINT A_CONS = 8;
const UINT A_s2_confrun = 3;
const UINT A_s2_reconf = 2;
const UINT A_s1_if = 30;
const UINT A_S_RETURN = 31;
const UINT A_s5_return = 31;
const UINT A_s3_run = 1;
const UINT A_s1_reconf = 2;
const UINT A_s5_reconf = 2;
const UINT A_s4_return = 31;
const UINT A_plus = 9;
const UINT A_lt = 13;
const UINT A_not = 21;
const UINT A_ASSIGN = 1;
const UINT A_APPEND = 9;
const UINT A_s4_run = 1;
const UINT A_over = 12;
const UINT A_RETURN = 1;
const UINT A_UPDATE = 3;
const UINT A_S_RUN = 1;
const UINT A_s1_run = 1;
const UINT A_s3_reconf = 2;
const UINT A_S_IF = 30;
const UINT A_s5_if = 30;
const UINT A_s2_return = 31;
const UINT A_s2_run = 1;
const UINT A_s4_if = 30;

} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/

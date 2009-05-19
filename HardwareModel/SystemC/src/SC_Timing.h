/*
 * SC_Timing.h
 *
 *  Created on: 12-Nov-2008
 *      Author: StudentAdmin
 */

#ifndef SC_TIMING_H_
#define SC_TIMING_H_


//#define SBA_TUNIT   SC_NS   //!< The default time unit
//#define TP_CLOCK    10      //!< clock time period

#define _CLK_P 10        // Clock Period
#define _CLK_U SC_NS     // Unit of Clock Period


#define _UNIT_WAIT_FUNC     wait(_CLK_P, _CLK_U) // macro for waiting one time unit

// ================================================================================
// Wait times for various time-consuming operations in the SBA (in _CLK_U)
// NOTE: The macro is for the complete function call. Will make the code cleaner.
// ================================================================================

// **********  SUBTASK_LIST ************

#define _STLIST_WRITE_DLY   wait(3 * _CLK_P, _CLK_U)    // Read, mod, write; so 3 clock cycles for write methods of subtask_list
#define _STLIST_READ_DLY    wait(1 * _CLK_P, _CLK_U)    // 1 clock cycle for read methods for subtask_list
#define _STLIST_ARG_DLY     wait(6 * _CLK_P, _CLK_U)    // Was 9: 4 clocks to read arguments (why?), 1 for mod, 4 for write back, in the argument() interface method
/*
 * Time to read arguments list:
 * 1. look up number of argument words: 1 cycle
 * 2. read each argument word & extract 2 args: nargs/2 cycles
 * 3. as many again to write back
 * So we have 2+nargs cycles or 2+arguments.size(). For 4 args this means 6, not 9
 * 
 * */





#endif /* SC_TIMING_H_ */

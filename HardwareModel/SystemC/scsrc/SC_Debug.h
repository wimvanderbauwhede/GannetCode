/*
 * SC_Debug.h
 *
 *  Created on: 05-Nov-2008
 *      Author: StudentAdmin
 */

#ifndef SC_DEBUG_H_
#define SC_DEBUG_H_

#include <iostream>
#include <cstring> // to use c_str()
#include <string>

//#define SC_DEBUG            // define to turn on debug messages





ofstream OSTREAM; // for storing output debug messages (link to file in sc_main)
ofstream CON; // for storing connection info (link to file in sc_main)

// out stream where debug messages should be put
//#define OSTREAM std::cout // select this for viewing messages on screen
//#define OSTREAM OUT         // this for storing output in log file
//#define OSTREAM  cout         // this for storing output in log file

namespace SC_SBA{

//==============================================================================
//  Functions for Debug messages
//==============================================================================

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : const_debug_msg
//* Object              : debug message at creation time
//* Input Parameters    : name() and kind() of module
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

void const_debug_msg(const char* name, const char* kind)
{
    #ifdef SC_DEBUG
        OSTREAM << "Constructing: "
              << std::setw(60) << setfill('-') << left <<  name
              << "\tof type:\t"
              << kind << endl;
                //name() and kind() are sc_object methods
    #endif //SC_DEBUG
}


//*---------------------------------------------------------------------------------------------------------
//* Function Name       : run_debug_msg
//* Object              : debug message at when a module runs it main run()/do_proc() method
//* Input Parameters    : name() of module
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------

void run_debug_msg(const char* name)
{
    #ifdef SC_DEBUG
        OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp()
                << ": "
                << std::setw(60) << setfill('-') << left <<  name
                << "\tmodule is now running" << endl;
    #endif //SC_DEBUG
}


//*---------------------------------------------------------------------------------------------------------
//* Function Name       : trying_read_write_msg
//* Object              : debug message at when a module is trying to read or write
//* Input Parameters    : write: 1 = trying to write, 0 = trying to read
//*                       source: name() of data source
//*                       sink: name() of data sink
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
void dbg_trying_read_write_msg(bool write, const char* source, const char* sink )
{
    #ifdef SC_DEBUG
        // if trying to write
        if(write)
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << std::setw(60) << setfill('-') << left <<  source
            << "\tis trying to write to\t: " << sink << endl ;
        // else trying to read
        else
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << std::setw(60) << setfill('-') << left <<  sink
            << "\tis trying to read from\t: " << source << endl ;
    #endif//SC_DEBUG
}

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : read_write_msg
//* Object              : debug message at when a module has read or written (i.e. read/write op is complete)
//* Input Parameters    : write: 1 = trying to write, 0 = trying to read
//*                       master: name() of master
//*                       slave : name() of slave
//*
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
void dbg_read_write_msg(bool write, const char* source, const char* sink)
{
    #ifdef SC_DEBUG
        // if writing
        if(write)
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << std::setw(60) << setfill('-') << left <<  source
            << "\thas written to\t\t\t: " << sink << endl ;
        // else reading
        else
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << std::setw(60) << setfill('-') << left <<  sink
            << "\thas read from\t\t\t: " << source << endl ;
    #endif//SC_DEBUG
}

//*---------------------------------------------------------------------------------------------------------
//* Function Name       : dbg_general_msg
//* Object              : general debug message (accepts a string and displays it in the correct format)
//* Input Parameters    : source    : name() of module creating this message
//*                       str*      : string to print
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
void dbg_general_msg (const char* source, const char* str)
{
    #ifdef SC_DEBUG
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            << std::setw(60) << setfill('-') << left <<  source
            << "\t" << str << endl ;
    #endif//SC_DEBUG
}


//*---------------------------------------------------------------------------------------------------------
//* Function Name       : dbg_show_module_connections
//* Object              : show the connections of a module
//* Input Parameters    :
//* Output Parameters   : None
//*---------------------------------------------------------------------------------------------------------
void dbg_show_module_connections()
{
#ifdef SC_DEBUG



#endif//SC_DEBUG
}










}//namespace SC_SBA

#endif /* SC_DEBUG_H_ */

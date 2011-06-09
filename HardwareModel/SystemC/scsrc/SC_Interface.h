/*
********************************************************************************
                 |
  File Name      | SC_Interface.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of the Gannet SoC Platform
-----------------|--------------------------------------------------------------
  Created        | 09-Feb-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Module for the Interface object
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081029: Created.
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_INTERFACE_H_
#define SC_INTERFACE_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_SBA.h"

#ifndef NO_SOCKET
#include "../../C++/GannetSocket/Server.h"
#endif

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------

#ifndef NO_SOCKET
using namespace GannetSocket;
#endif

using namespace std;

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------

//==============================================================================
//  CLASS: THE SBA INTERFACE CLASS
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Module for the Interface object
    /*!
       Detailed description here...
    */
class SC_Interface : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------
    port_SC_reg_if<uint	>   io_mech;
    // task description through a port? Can't we simply pass it as a constructor
    // argument like its done for the rest (like SC_Gateway??)

    // ---------------------------- Non-SC types ------------------------------
    SBA::TaskDescList		tasks;
	uint io_mech_loc; // FIXME
#ifndef NO_SOCKET
          Server gserver;
#endif
        uint iodescs[MAX_NTASKS];
		BytecodeQueue tdcs;
	// ---------------------------- SC-Sub-Modules -----------------------------

	// ---------------------------- METHODS ------------------------------------
#ifndef NO_SOCKET
	Bytecode read_bytecode(uint status);
#endif
	Bytecode read_bytecode(string);
	uint receive(uint);
	void send(Word_List&,uint);

    void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Interface"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Interface);
    SC_Interface(  sc_module_name nm , SBA::TaskDescList& tds_  ) :
        sc_module	(nm),
        tasks (tds_),
        io_mech_loc(1)
    {
        SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
    }//Constructor
};/* class: SC_Interface */

//==============================================================================
//  DO_PROC()
//==============================================================================
void SC_Interface :: do_proc()
{
    //debug message
    //run_debug_msg(name());

}// funct: SC_Interface :: do_proc()




#ifndef NO_SOCKET
Bytecode SC_Interface::read_bytecode(uint status){ //H
        Bytecode bytewords;

        bytewords = gserver.run(status);
#ifdef VERBOSE
        cout<<"****** Begin gserver ******"<<endl;
        cout<<"Interface/gserver.run(): bytewords[0]="<<bytewords.at(0) << endl;
        cout<<"Interface/gserver.run(): bytewords[1]="<<bytewords.at(1) << endl;
#endif //VERBOSE
        return bytewords;
 }
#endif

 Bytecode SC_Interface::read_bytecode(string tdc_file) {
         FILE * fd=fopen(tdc_file.c_str(),"r");
         Bytecode bytewords;
        Word byteword=0;
        uint hwb=0;
         int byte=0;
         while(byte!=EOF) {
         byte=fgetc(fd);
            byteword+=(byte<<(8*(NBYTES-1-hwb)));
            hwb=hwb+1;
            if (hwb==NBYTES){
                hwb=0;
                bytewords.push_back(byteword);
                byteword=0;
            }
         }
        return bytewords;
    }


 uint SC_Interface::receive(uint core_status) {
// 	cout << "VMIF receive():\n";
        if (io_mech_loc==(uint)0 ){
        	cerr << " VMIF SOCKET IO, unsupported\n";
        	exit(0);
            uint mode=0;
            if (core_status == CS_idle ){
                mode=1;
            }
            return 0;
        } else {
            if (tasks.size()>0){
                 StringPair tdc_file=tasks.front();tasks.pop_front();
                 Bytecode bycl=read_bytecode(tdc_file.taskfile);tdcs.push(bycl);
                 // datafile is extracted in SC_System constructor
                return 1;
            } else {
                return 0;
            }
        }
    }

 //==============================================================================
//  send()
//==============================================================================
void SC_Interface::send(Word_List& result, uint taskid) {
	OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": DONE\n";
	//OSTREAM << "DONE: "<< sc_time_stamp() << "\n";
#ifndef NO_SOCKET
	if (io_mech == 0) {
		uint fd = iodescs[taskid]; // part of Interface obj
	} else {
#endif
		if (result.size() > 1) {
			Word_List result_payload = result;
#ifdef VERBOSE
			OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() <<": RESULT (VMIF): ( ";
#endif // VERBOSE
			if (result.size() == 2) {
				deque<Int> int_result = to_signed_int_list(result_payload);
				bool first = true;
				const Int lo = -0x22FFFFFFL;
				const Int hi = 0xDD000000L;
				for (deque<Int>::iterator iter_ = int_result.begin(); iter_
						!= int_result.end(); iter_++) {
					Int elt = *iter_;
					if ((elt > lo and elt < hi) or not first) {
						OSTREAM << elt << " ";
						first = false;
					}
				}
			} else {
				for (Word_List::iterator iter_ = result.begin(); iter_
						!= result.end(); iter_++) {
					Word elt = *iter_;
					OSTREAM << elt << "\n";
				}

			}
#ifdef VERBOSE
			OSTREAM << ")";
#endif // VERBOSE
			OSTREAM << "\n";
		} else {
			deque<Int> int_result_list = to_signed_int_list(result);
			Int int_result = int_result_list[0];
			OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp()
					<< ": RESULT (VMIF): " << int_result << "" << endl;
		}

#ifndef NO_SOCKET
	}
#endif
} // send()

} /* namespace: SC_SBA */

#endif /* SC_INTERFACE_H_ */

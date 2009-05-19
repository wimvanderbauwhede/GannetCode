#ifndef GEN_SYSC
#include "../src/SC_sba.h"
#else
#include "../scsrc/SC_sba.h"
#endif

int sc_main(int ac, char*av[])
{
    // open file for writing debug messages...
#if OSTREAM != cout
        OSTREAM.open("OUT.log");
#endif    
    
    string tdc_file="NONE";
    unsigned int ncycles=0;
    if(ac>1)
    {
        tdc_file=av[1];//args.at(0);
        if(ac==3)
        {
            ncycles=atoi(av[2]);
        } else if (ac>3) {
            cout << "Usage: tdc_file [time in us]\n"; // change message for SC model
            exit(-1);
        }
    }

    if (tdc_file=="NONE")
    {
        cout << "Usage: tdc_file [time in us]\n"; 
        exit(-1);
    }

    string data_file=tdc_file;
    unsigned int base_str_len=data_file.size()-4;
    data_file.resize(base_str_len);
    data_file+=".data";

    StringPair sp;
    sp.Taskfile=tdc_file;//"../../Tasks/task_pre_alu.tdc";
    TaskDescList tds;
    tds.push_back(sp);

	// create SC_Runtime object and pass it the TaskDescList created from
	// the input file.
	SC_SBA::SC_Runtime Runtime("Runtime", tds);

	sc_report_handler::set_actions(SC_ID_OBJECT_EXISTS_,SC_DO_NOTHING);
	sc_report_handler::set_actions(SC_ID_ILLEGAL_CHARACTERS_,SC_DO_NOTHING);
    if (ncycles>0) {
        sc_start(ncycles*1000,SC_NS);
    } else {
        sc_start();
    }
	return 0;
}

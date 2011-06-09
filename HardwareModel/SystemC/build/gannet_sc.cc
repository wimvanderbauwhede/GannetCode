#ifndef GEN_SYSC
#include "../src/SC_sba.h"
#else
#include "../scsrc/SC_SBA.h"
#endif
namespace SC_SBA {
    string data_file;
    string yaml_file;
}

/*
 To use YAML inside the SystemC model, we must get the YAML file name 
 So we should maybe have proper command-line arguments just like in Ruby.
 Failing that we can assume that the .td file is in the same folder as the .tdc file,
 and get the YAML filename from there. 
 That assumes the use of PCRE.
 OR we could store the YAML file name in the .tdc file, as bytes.
 that means a larger .tdc file, but we could have a flag to strip it out.

 */
int sc_main(int ac, char*av[])
{
    // open file for writing debug messages...
    

    string tdc_file="NONE";
    string data_file="NONE";
    string log_file="NONE";
    unsigned int ncycles=0;
// 2 -> tdc; 3 -> tdc ncycles; 4-> tdc ncycles data
    if(ac>1)
    {
        tdc_file=av[1];//args.at(0);
//#if OSTREAM != cout
        log_file=tdc_file;
        unsigned int base_str_len=log_file.size()-4;
        log_file.resize(base_str_len);
        log_file+=".log";
        OSTREAM.open(log_file.c_str());
//#endif
        if(ac>=3)
        {
            ncycles=atoi(av[2]);
        } 
	if (ac==4) {
            data_file=av[3];                    
        } else if (ac>4) {
            cout << "Usage: tdc_file [time in us] [datafile]\n"; 
            exit(-1);
        }
    }

    if (tdc_file=="NONE")
    {
        cout << "Usage: tdc_file [time in us] [datafile]\n"; 
        exit(-1);
    }

  /*  
    unsigned int base_str_len=data_file.size()-4;
    data_file.resize(base_str_len);
    data_file+=".data";
*/
    StringPair sp;
    sp.taskfile=tdc_file;//"../../Tasks/task_pre_alu.tdc";
    sp.datafile=data_file;//"../../Tasks/task_pre_alu.data";
    SC_SBA::data_file=data_file;
    TaskDescList tds;
    tds.push_back(sp);

	// create SC_Runtime object and pass it the TaskDescList created from
	// the input file.
	SC_SBA::SC_Runtime Runtime("Runtime", tds);

	sc_report_handler::set_actions(SC_ID_OBJECT_EXISTS_,SC_DO_NOTHING);
	sc_report_handler::set_actions(SC_ID_ILLEGAL_CHARACTERS_,SC_DO_NOTHING);
    if (ncycles>0) {
    cout << "Running for "<<ncycles<<"us\n";
        sc_start(ncycles*1000,SC_NS);
    } else {
        sc_start();
    }
//#if OSTREAM != cout
    OSTREAM.close();
//#endif
	return 0;
}

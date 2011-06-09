# Interface.rb  
# 
# :title: Gannet Service-based SoC project - Interface class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: Interface.rb 2253 2009-02-17 17:35:14Z socgroup $

=begin #inc
#include <queue> //skiph
#ifdef WV_SYSTEMC
#include <systemc.h> //skipcc
#endif
#ifdef TIMINGS
#include "Timings.h"
#endif // TIMINGS                            
#include "Base/System.h" //skipcc
#include "System.h" //skiph
#include "TaskDescription.h" //skipcc
#include "Types.h" //skipcc
#include "ServiceConfiguration.h" //skipcc
#include "Interface.h" //skiph
#ifndef NO_SOCKET
#include "../GannetSocket/Server.h" //skipcc
#endif // NO_SOCKET
=end #inc

require "SBA/TaskDescription.rb"

#ifndef NO_SOCKET
#C++ using namespace GannetSocket;
#endif // NO_SOCKET 

#ifdef NO_FILE_IO
#C++ namespace SBA { //skipcc
#C++ extern Word byc_array[]; //skipcc
#C++ extern unsigned int byc_size; //skipcc
#C++ } //skipcc
#endif // NO_FILE_IO

# Interface to file or socket
class SBA_Interface 

    include  SBA
#ifndef NO_SOCKET    
    #H      Server gserver;
#endif // NO_SOCKET    
    #H    	Base::System* sba_system_ptr;
    #H    	Base::Tile* sba_gwtile_ptr;
    #H      uint iodescs[MAX_NTASKS];
    
    attr_accessor :tdcs,#t BytecodeQueue;        
                :iodescs #skip
    #skipcc
=begin #constructor
#ifdef TIMINGS
        double t_start;
        double t_stop;                
#endif                                
 Interface(Base::System* sba_s_, Base::Tile* sba_t_) 
		:  sba_system_ptr(sba_s_), sba_gwtile_ptr(sba_t_)
		  {                		      
		  };
=end #constructor
    #endskipcc
    
    #skip    
    # object constructor
    def initialize(sba_gw,sba_system)
        @sba_system=sba_system   
#        @sba_system.io_mech=2 # as we use file IO for now
        @tdcs=[]
        @iodescs=[]
        @v=(VERBOSE==1)    
    end
    #endskip

=begin #C++
    
Bytecode Interface::read_bytecode(uint status){ //H
        Bytecode bytewords;
#ifndef NO_SOCKET        
        bytewords = gserver.run(status);
#ifdef VERBOSE
        cout<<"****** Begin gserver ******"<<endl;
        cout<<"Interface/gserver.run(): bytewords[0]="<<bytewords.at(0) << endl;
        cout<<"Interface/gserver.run(): bytewords[1]="<<bytewords.at(1) << endl;
#endif //VERBOSE
#endif // NO_SOCKET    

    return bytewords;
 } 
=end #C++

        
    def read_bytecode(tdc_file) #t Bytecode (string)
#ifndef NO_FILE_IO        
#        puts "read_bytecode()"
        tdch=File.open(tdc_file,"r") #C++ FILE * fd=fopen(tdc_file.c_str(),"r");
        bytewords=[] #C++ Bytecode bytewords;
        byteword=0 #t Word
        hwb=0 #t uint
        #C++ Word byte=0; 
        #C++ while((Int)byte!=EOF) {
        tdch.each_byte {|byte| #C++ byte=(Word)fgetc(fd);  
            byteword+=(byte<<(8*(NBYTES-1-hwb)))
            hwb=hwb+1
            if hwb==NBYTES
                hwb=0
                bytewords.push(byteword)
                byteword=0
            end            
        } #C++ }
#        puts "exit read_bytecode()"
#else
=begin #C++ 
        // we assume bytecode is stored in the extern byc_array, a static array of extern byc_size
        Bytecode bytewords;
        for (uint i=0;i<byc_size;i++) {
            bytewords.push_back(byc_array[i]);
        }
=end #C++        
#endif // NO_FILE_IO                
        return bytewords
    end    

    
    # ------------------------------------------------------------------------------
    #
    # Main methods
    #
    
    # At every timestep, this function checks for events:
    #   -is there a packet for me?
    #   -has service processing finished? 
    def receive(core_status) #t uint
    #C++ System& sba_system=*((System*)sba_system_ptr);
        if @sba_system.io_mech==0 # socket IO
            mode=0 #t uint # poll
            if core_status == CS_idle 
                mode=1 # block
            end
            return 0
        else # io_mech==1
#            puts "io_mech must be 1"
                       
            if @sba_system.task_descriptions.length==1
#ifdef TIMINGS
#C++            t_start=wsecond(); 
#endif 
                #WV20112009: I don't see why I should not remove the task from the task_descriptions if there's only one
#                if @sba_system.task_descriptions.length>1
                    tdc_file=@sba_system.task_descriptions.shift #C++ StringPair tdc_file=sba_system.task_descriptions.front();sba_system.task_descriptions.pop_front();
#                else
#                    tdc_file=@sba_system.task_descriptions[0] #C++ StringPair tdc_file=sba_system.task_descriptions.front();                    
#                end
                @sba_system.task_data=tdc_file[1] #s/file.../file.datafile/    
                @tdcs.push(read_bytecode(tdc_file[0])) #C++ Bytecode bycl=read_bytecode(tdc_file.taskfile);tdcs.push(bycl);                    
                @tdcs.inspect #skip                  
#                puts "done read_bytecode()"      
                return 1
            elsif @sba_system.task_descriptions.length>1 #skip
                raise "Only a single task bytecode and a single task data file can be specified on command line" #skip
            else
#                puts "no task description"
                return 0
            end                                
        end # io_mech
    end # of receive()
    # ==============================================================================
    def send(result,taskid) #t void (Word_List&;uint)
    #C++ System& sba_system=*((System*)sba_system_ptr);
       
        if @sba_system.io_mech==0 # socket IO                
#ifndef NO_SOCKET 
#            fd=@iodescs[taskid] #t uint
#C++        gserver.sendResultToClient(result);            
#endif // NO_SOCKET   
        else # io_mech==1
#ifdef TIMINGS
#C++                t_stop=wsecond();
#C++                cout << t_stop-t_start << "\n";
#else                     

                if result.length>1                    
                    result_payload=result #t Word_List
                    #iv
                                        print "RESULT (VMIF): ( " 
                    #ev                    
                    if FP==0
                        int_result=to_signed_int_list(result_payload) #t deque<Int>
                        first=true #t bool
                        for elt in int_result #t deque<Int>
#                            if (elt > -0x22FFFFFF and elt < 0xDD000000) or not first
                             
                            if first and elt == 0xcd000100
                            #iv   
                                print "0x#{sprintf('%x',elt)} " if @v #skip
                                #C++ cout << "0x" << hex << elt << " ";
                            #ev                                    
                            else
                                print elt, " " #C++ cout << dec << elt << " ";
                            end
                            first=false
                        end
                    else # FP==1
                        flt_result=to_float_list(result_payload) #t Double_List
                        first=true #t bool
			            for elt in flt_result #t Double_List
                              if first
#                                  print "0x#{sprintf('%x',elt)} "  #C++ cout << "0x" << hex << elt << " ";
                                  first=false
                              else
                                  print elt, " " #C++ cout << dec << elt << " ";
                              end
                        end
                    end # FP
#iv
                    print ")" 
#ev                    
                    print "\n"         
                elsif result.length==0
                    puts "RESULT (VMIF): []"           
                else
                    # We assume that a single returned Word must be a 16-bit integer
                    puts "RESULT (VMIF): #{getValue(result[0])}"                     
#                    if FP==0
#                        int_result_list=to_signed_int_list(result) #t deque<Int>
#                        int_result=int_result_list[0] #t Int
#                        puts "RESULT: #{int_result}" 
#                    else # FP==1
#                        flt_result_list=to_float_list(result) #t Double_List
#                        flt_result=flt_result_list[0] #t double
#                        puts "RESULT: #{flt_result}" 
#                    end # FP                    
                end
#endif // TIMINGS              
        end # io_mech
    end # of send()    
    # ==============================================================================
    
end # of SBA_Interface class

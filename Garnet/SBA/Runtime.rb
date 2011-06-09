# SBA/Runtime.rb
#   
# :title: Gannet Service-based SoC project - SBA Runtime class
#
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#//==============================================================================
#//
#// Gannet Service-based SoC project - SBA Runtime class 
#//
#//==============================================================================
#
#// $Id: Runtime.rb 2155 2009-01-28 11:39:41Z socgroup $
#++

=begin #inc
#ifdef WV_SYSTEMC
#include <systemc.h> //skipcc
#endif // WV_SYSTEMC
#include "Types.h" //skipcc
#include "SystemConfiguration.h" //skipcc
#include "System.h" //skipcc
#include "Runtime.h" //skiph
=end #inc

=begin

=end

# WV20110609: for the old configuration behaviour revert to before r4987 and set NEW=0

require "SBA/ServiceConfiguration.rb"
require "SBA/SystemConfigurationNew.rb"
require "SBA/System.rb"

# SBA_Runtime is now a very thin wrapper around SBA_System

class SBA_Runtime
	include SBA_SystemConfiguration

#H TaskDescList task_descriptions;	
#H Bytecode bytecode;
#H System sba;	
#H Runtime(TaskDescList& td_)	: task_descriptions(td_) ,
#H       sba(task_descriptions)
#H {};
#H Runtime(Bytecode& byc_)	: bytecode(byc_) ,
#H       sba(bytecode)
#H {};
    #skip    
    # object constructor	
	def initialize(task_descriptions)      
		@task_descriptions=task_descriptions		
        @sba=SBA_System.new(self.servicenodes,self.configurations,@task_descriptions)
	end
   
	def show()
	    i=0 #t uint
		for task in @task_descriptions 
		    i=i+1
		    puts "Task #{i}"
        		puts "------"
                if task[0][0].is_a?(SBA_Packet) 
                for subtask in task[0]
                puts "===="
                puts subtask
                end
                else
        		puts task
        		end
		    puts
		end
	end
    #endskip
	
	def run(ncycles=500) #t Word_List& (uint) #s/=...//
#iv
		puts "=========================================================================="
		puts "Running ..."
#ev
if USE_THREADS==0            
        t=0 #t uint
        while t<ncycles
            t=t+1
            #iv
            puts " *** STEP #{t} ***"
            #ev
            @sba.run()		 
            if @sba.finished == 1                    
                return @sba.results
                # break
            end            
        end
        puts "Warning: GannetVM ran for #{ncycles} cycles without completion." #skiph
        puts "         Try increasing the number of cycles on the command line." #skiph
else #  USE_THREADS==1
#    The current Gannet thread model is to create a thread per tile and use blocking queues.
#    This should work fine for long-running SW tasks as the scheduler will timeslice the threads.
#    What about HW services? 
#    As long as there is a single HW service per Tile, the thread can simply block until the HW returns a result
#    so the HW response will be the scheduler period. It means we don't even need an interrupt from the HW
#    When do we really need interrupts? It would be logical to let the core deal with the interrupts for its HW    
        @sba.run_th()
end # USE_THREADS   
        return @sba.results
	end

#H }; // Clas Runtime
#H } // SBA
	
end # of SBA_Runtime


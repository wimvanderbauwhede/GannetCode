#
# :title: Garnet Service-based SoC project - SBA TaskDescription class
#
#    This version of Gannet is for implementation in HW
#    This is the version to be ported to SystemC
#
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
# $Id: TaskDescription.rb 2155 2009-01-28 11:39:41Z socgroup $

=begin #inc

#include "ServiceConfiguration.h" //skipcc
#include "Packet.h" //skipcc
#include <vector> 
#include "TaskDescription.h" //skiph

=end #inc

# This class turns Gannet bytecode into a list of SBA_Packets

class SBA_TaskDescription
   include SBA

    attr_reader :Packets, #t Packet_List; #WV30072008: This is limited to PACKET_FIFO_SZ, so most likely too short!
                :NPackets #skip

#H  TaskDescription(string,uint);
#H  TaskDescription(Bytecode&,uint);

#skiph
=begin #constructor

	//Constructor
	TaskDescription::TaskDescription(string tdc_file,uint task_id) {
    	byc2td(tdc_file);
    }; // constructor
	TaskDescription::TaskDescription(Bytecode& tdc_wl,uint task_id) {
    	wl2td(tdc_wl);
    }; // constructor    
=end #constructor
#endskiph

#skip        
    def initialize(byc_list_or_tdc_file,task_id) #t string
            @v=(VERBOSE==1)
        @Packets=[]        
        if byc_list_or_tdc_file.is_a?(Array)
#            raise "Garnet only supports file IO, not Word_List"
            wl=byc_list_or_tdc_file            
            wl2td(wl)
        elsif byc_list_or_tdc_file.is_a?(String)
            @tdc_file=byc_list_or_tdc_file
            byc2td(@tdc_file)
        else
            raise "SBA_TaskDescription::initialize: wrong argument type to constructor", packet_list_or_tdc_file
        end
    end
#endskip
     
    #--------------------------------------
    def byc2td(tdc_file) #t void (string)
#C++ cout << tdc_file << endl;
        fd=read_bytecode(tdc_file) #C++ FILE * fd=fopen(tdc_file.c_str(),"r");
        npackets=0 #t uint
    	b0=0 #t uint16        
    	for ii in 0..NBYTES-1 #t uint	
    		byte=fd.shift #C++ unsigned short int byte=fgetc(fd);
    		if ii==0
    			b0=byte
    		elsif ii==1 
    			npackets=(b0 << 8)+byte
    		end
    	end
    	#iv
    	puts "NPackets: #{npackets}"
    	#ev
        for np in 0..npackets-1 #t uint
    		header_words=[] #C++ Header_t header_words;//Word_List header_words(3);    	
    	    header_words=byc2header(fd)
             length=(header_words[0] & F_Length) >>FS_Length #t Uint16   	
             header= header_words #C++ Header_t header(header_words);
    
            # extract payload bytewords and put into array nw
             payload=byc2playload(length,fd) #t Word_List
             packet=mkPacket(header,payload) # VMIF
#iv
                      puts "========"
                      puts ppPacket(packet)
                      puts "-- #{packet.inspect}" #skip
#ev                      
             @Packets.push(packet) #s/push/push_back/
        end # loop over all packets
#C++        fclose(fd); 
    end

    #--------------------------------------
    def wl2td(tdc_wl) #t void (Bytecode&)

    	npackets = tdc_wl.shift #C++ unsigned int npackets=(unsigned int)tdc_wl.front();tdc_wl.pop_front();
    	npackets = npackets >> 16;
    	#iv
    	puts "NPackets: #{npackets}"
    	#ev
    	puts tdc_wl.inspect if @v #skip
        if npackets==0
            exit(0)
        end            
        for np in 0..npackets-1 #t uint
    		header_words=[] #C++ Header_t header_words;    	
        	for phw in 0..2 #t uint
            	hw=tdc_wl.shift #C++ Word hw=tdc_wl.front();tdc_wl.pop_front();
                header_words.push(hw) #s/push/push_back/
    	     end    		
    	     
             length=(header_words[0] & F_Length) >> FS_Length #t uint   	
             header= header_words #C++ Header_t header(header_words);
            #iv
#            puts "Header:\n#{ppHeader(header)}"
            puts "Length: #{length}"
            #ev
    
            # extract payload bytewords and put into array nw
            payload=[] #t Word_List #s/=..//
            for j in 0..length-1 #t uint
                plw=tdc_wl.shift #C++ Word plw=tdc_wl.front();tdc_wl.pop_front();
                payload.push(plw) #s/push/push_back/
            end        
                     
             packet=mkPacket(header,payload) 
#iv
                      puts "========"
                      puts ppPacket(packet)
#ev                      
            @Packets.push(packet) #s/push/push_back/
                        
        end # loop over all packets
    end

    #--------------------------------------
    
#skip    
    def read_bytecode(tdc_file)
        tdch=File.open(tdc_file,"r")
        bytes=[]
        tdch.each_byte {|i|
            bytes.push(i)
        }
        return bytes
    end
#endskip

    def byc2header(fd) #t Header_t (FILE*)
    	header_words=[] #C++ Header_t header_words; // Word_List header_words(3);
    	for phw in 0..2 #t uint
        	hw=0 #t Word
	        for hwb in 0..NBYTES-1 #t uint
	            byte=fd.shift #C++ Uint64 byte=fgetc(fd);
                hw+=(byte<<(8*(NBYTES-1-hwb)))
            end
            header_words.push(hw) #s/push/push_back/
	     end
	     return header_words
    end # of byc2header()

    def byc2playload(length,fd) #t Word_List (Uint16;FILE*)
        payload=[] #t Word_List #s/=..//
        for j in 0..length-1 #t int
            plw=0 #t Word
            for plwb in 0..NBYTES-1 #t uint
                byte=fd.shift #C++ Word byte=fgetc(fd);
                plw+=(byte<<(8*(NBYTES-1-plwb)))        
            end
            payload.push(plw) #s/push/push_back/
        end
        return payload
    end # of byc2payload()
    
end # of SBA_TaskDescription

#
# :title: Garnet Service-based SoC project - SBA TaskDescription class
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# $Id: TaskDescription.rb 2155 2009-01-28 11:39:41Z socgroup $

=begin #inc

#include "ServiceConfiguration.h" //skipcc
#include "Packet.h" //skipcc
#include <vector>
#include <stdio.h> 
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
    # OBSOLETE?
    def byc2td(tdc_file) #t void (string)
#C++ cout << tdc_file << endl;
        fd=read_bytecode(tdc_file) #C++ FILE * fd=fopen(tdc_file.c_str(),"r");
        npackets=0 #t uint
    	b0=0 #t uint
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
        subtask_packet=[] #t Word_List
        for np in 0..npackets-1 #t uint
    		header_words=[] #C++ Header_t header_words;    	
    	    header_words=byc2header(fd)
            length=getLength(header_words) #t Uint16
            ptype= getPacket_type(header_words) #t Packet_type_t                   	
            header= header_words #C++ Header_t header(header_words);    
            # extract payload bytewords and put into array nw
            payload=byc2playload(length,fd) #t Word_List
            packet=mkPacket(header,payload) # VMIF
#iv
                      puts "========"
                      puts ppPacket(packet)
                      puts "-- #{packet.inspect}" #skip
#ev                      
            if (ptype==P_subtask)
                subtask_packet=packet
            else
                 @Packets.push(packet)
            end
        end # loop over all packets
        @Packets.push(subtask_packet)
        for packet in @Packets #skip
            puts ppPacket(packet) #skip
        end #skip       
#C++        fclose(fd); 
    end

    #--------------------------------------
    def wl2td(tdc_wl) #t void (Bytecode&)
       
    	npackets = tdc_wl.shift #C++ Word npackets=(Word)tdc_wl.front();tdc_wl.pop_front();
        #iv
         puts "NPackets Word: #{npackets}" if @v #skip
         #ev
    	npackets = npackets >> (NBYTES*8-16); # 32 -> 16; 64 -> 48  
    	#iv
    	puts "NPackets: #{npackets}" if @v #skip
    	#ev
    	puts tdc_wl.inspect if @v #skip
        if npackets==0
            exit(0)
        end            
        # Every program contains exactly one packet of type P_subtask, all others are P_code
        # The code packets should be sent first, so the subtask packet is move to the end of the list
        subtask_packet=[] #t Word_List
        for np in 0..npackets-1 #t uint
    		header_words=[] #C++ Header_t header_words;    	
        	for phw in 0..2 #t uint
            	hw=tdc_wl.shift #C++ Word hw=tdc_wl.front();tdc_wl.pop_front();
                header_words.push(hw)
    	     end    		
             length=getLength(header_words) #t uint
             ptype= getPacket_type(header_words) #t Packet_type_t                    
             header= header_words #C++ Header_t header(header_words);
            #iv
            puts "Length: #{length}" if @v #skip
            #ev
    
            # extract payload bytewords and put into array nw
            payload=[] #t Word_List #s/=..//
            for j in 0..length-1 #t uint
                plw=tdc_wl.shift #C++ Word plw=tdc_wl.front();tdc_wl.pop_front();
                payload.push(plw)
            end        
                     
             packet=mkPacket(header,payload) 
#iv
                      puts "========" if @v #skip
                      puts ppPacket(packet) if @v #skip
#ev                      
            if (ptype==P_subtask)
                subtask_packet=packet
           else
                @Packets.push(packet)
           end                        
        end # loop over all packets
        @Packets.push(subtask_packet)
             
        # If the bytecode contains the path to the YAML file, it should be in the remaining Words
#skip        
        ymlfile="" #t string
        w=[] #C++ char w[NBYTES];
        for word in tdc_wl #t Word_List
            for i in 0..NBYTES-1 #t uint
                w[i]=word>>((NBYTES-1-i)*8)&0xFF
                if w[i]!=0
                    #C++ char cw=w[i];
                    ymlfile+=w[i].chr() #C++ ymlfile+=string((char*)&cw); 
                end
            end            
            # ymlfile+=0.chr() #skip
        end
        #iv
        puts "YAML file name: <#{ymlfile}>" if @v #skip
        #ev
#endskip       
    end

    #--------------------------------------
    #skip
    def make_packet_list
        # TODO
    end # make_packet_list()
    #endskip
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
            header_words.push(hw)
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
            payload.push(plw)
        end
        return payload
    end # of byc2payload()
    
end # of SBA_TaskDescription

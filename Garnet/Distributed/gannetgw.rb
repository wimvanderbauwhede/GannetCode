#!/usr/bin/env ruby 

#/** \file garnet
#   
# \brief Service-based SoC project 
#
#*/
#
# (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
#//==============================================================================
#//
#// Service-based SoC project - Garnet, the Ruby Gannet
#//
#//==============================================================================

if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
    @@gannet_dir="#{ENV['GANNET_DIR']}"
else    
    raise "Please set GANNET_DIR=[path to your Gannet directory]"
end

help= <<EOH

    Usage: gannetgw.rb [-hnD] task description file [data file]
    -n, --@@ncycles: Run for this number of clock cycles
    -D, --datafile: input data (typically an image file)
EOH

require 'optparse'
opts=OptionParser.new
DISTR=1
NEW=1

@@ncycles=500
opts.on("-n VAL","--@@ncycles=VAL",Integer) {|val| @@ncycles=val }
WORDSZ=64
opts.on("-W VAL","--wordsz=VAL",Integer) {|val| WORDSZ=val }
    
USE_THREADS=0
SEQVM=0
VM=1 
DEBUG_ALL=0
QUIT=0
TO_YAML=0

@@ymlfile='SBA.yml'
opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| @@ymlfile=yml_file }
task_data=""
opts.on("-D datafile","--data-file",String) {|data_file| task_data = data_file }
opts.on("-h","--help") {
    puts help
    exit
}
args=opts.parse(ARGV)

@@multi_ip=args.shift.to_i
VERBOSE=args.shift.to_i
td_file=args.shift

require "SBA/ServiceConfiguration.rb"
require "SBA/GatewayTile.rb" 

class SBA_System
    attr_accessor :io_mech, :multi_ip
    def initialize()
        @io_mech=1
        @multi_ip=@@multi_ip
    end
end

def main(td_file,task_data)
    if @@ymlfile=='SBA.yml'
        maybe_yml_file=find_configpath(td_file)
        if File.exist?(maybe_yml_file)
            @@ymlfile=maybe_yml_file    
        elsif File.exist?( "#{@@gannet_dir}/SystemConfigurations/#{maybe_yml_file}")
            @@ymlfile="#{@@gannet_dir}/SystemConfigurations/#{maybe_yml_file}"    
        else
            raise "Could not find SystemConfiguration #{maybe_yml_file}"    
        end        
        puts "Found YAML-file #{@@ymlfile}" if VERBOSE==1           
    end 
    eval('SBA_YML=@@ymlfile')                   

    if td_file != 'NONE'
        task_descriptions=[]
        task_pair= [td_file]
        if task_data!=""
            task_pair.push(task_data)
        end
        task_descriptions.push(task_pair)
        sba_system=SBA_System.new()
        sba_system.task_descriptions=task_descriptions
        #Run   
        gwtile = SBA_GatewayTile.new(sba_system,0,0,task_descriptions)
        res=gwtile.run_proc(@@ncycles)    
    else 
            raise "Please specify a task description file"
    end

end # of main()

def find_configpath(tdc_file) 
    tdch=File.open(tdc_file,"r") 
    # first read all bytes into a list of Words
    bytewords=[] 
    byteword=0 
    hwb=0 
    tdch.each_byte {|byte| 
        byteword+=(byte<<(8*(NBYTES-1-hwb)))
        hwb=hwb+1
        if hwb==NBYTES
            hwb=0
            bytewords.push(byteword)
            byteword=0
        end            
    } 
    # Then make sense of the list of words:
    # Find number of packets
    npackets = bytewords.shift
    npackets = npackets >> (NBYTES*8-16)
    # For each packet
    for np in 0..npackets-1 
    # get the length from the header
        header_word =bytewords.shift
        for phw in 1..2 
            bytewords.shift
         end                         
         length=(header_word & F_Length) >> FS_Length    
         # read the payload
         for j in 0..length-1 
            bytewords.shift 
         end                                                     
    end 
    # If the bytecode contains the path to the YAML file, it should be in the remaining Words
    ymlfile=""
    w=[] 
    for word in bytewords 
        for i in 0..NBYTES-1
            w[i]=word>>((NBYTES-1-i)*8)&0xFF
            if w[i]!=0
                ymlfile+=w[i].chr()
            end
        end            
    end        
    return ymlfile               
end

main(td_file,task_data)


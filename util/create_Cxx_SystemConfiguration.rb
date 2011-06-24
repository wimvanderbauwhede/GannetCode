#!/usr/bin/ruby 

#/** \file garnet.rb
#   
# \brief Service-based SoC project 
#
#*/
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
# ==============================================================================
#
# Gannet project - create SystemConfiguration.h for C++ 
#
# ==============================================================================

#WV20110608: if you want the old behaviour back, revert to r4986 and set NEW=0 and NEWER=0
if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
else    
    $LOAD_PATH.push("#{ENV['HOME']}/SoC_Research/Code/Gannet/Garnet/")
end

help= <<EOH
    -S, --sysc: generate SC_SystemConfiguration.h for SystemC
    -Y, --yaml [YAML SystemConfiguration to be used as input]
    -D, --dir: relative path to directory for generated files
    -P, --procs: support processes+sockets
    -N, --new: for new features testing

EOH

require 'optparse'

opts=OptionParser.new()

@@sysc=0
@@prefix_SC=''
@@dirpath='./'
@@sba_dir='./'
SBA_YML='SBA.yml'
TO_YAML=0
USE_THREADS=0

VERBOSE=0
opts.on("-v","--verbose") {VERBOSE=1;}
opts.on("-S","--sysc") {@@sysc=1;@@prefix_SC='SC_'}
DISTR=0
opts.on("-P","--procs") {DISTR=1}

WORDSZ=64
opts.on("-W wordsz","--wordsz=wordsz",Integer) { |wordsz| WORDSZ=wordsz }
opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| SBA_YML=yml_file }
opts.on("-D dirpath","--dir=dirpath",String) {|dir_path| @@dirpath=dir_path }
opts.on("-C cwd","--cwd=cwd",String) {|cwd| @@sba_dir=cwd }
opts.on("-h","--help") {
puts help
exit
}
opts.parse(ARGV)

SBA_WD=@@sba_dir

require 'yaml'
require "SBA/ServiceConfiguration.rb"

def loadLibraryConfig(lib)
    if File.exists?("./Gannet/#{lib}.yml")
        libcfg=  YAML.load(File.open("./Gannet/#{lib}.yml"))
    elsif File.exists?("#{ENV['GANNET_DIR']}/SystemConfigurations/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{lib}.yml"))
    else
        raise "Can't find Library Config File #{lib}.yml"
    end
    return libcfg
end

#if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
#	cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))
#else
#	cfg = YAML.load(File.open(SBA_YML))
#end

appcfg =  YAML.load( File.open("#{SBA_YML}") )

@@sclibs=appcfg['System']['Libraries']

require "SBA/SystemConfigurationNew.rb"
  

# ====================================================================  
=begin
We need to read the libraries from the Application YAML file,
   then for each of these generate the tuples. 

=end


    def cxx_serviceclasses(appcfg)
        cxx_service_tuples=[] 
	    libs = appcfg['System']['Libraries']
	    for lib in libs
    	    libcfg =  loadLibraryConfig(lib)

	        for service_id_str in libcfg['System']['Services'].keys
    	        scid_wrapper=libcfg['System']['Services'][service_id_str] # BEGIN: [ 1, ls_BEGIN, 1 ]
        	    scid=scid_wrapper[0].to_i 
            	wrapper=scid_wrapper[1]
            
	            if @@sysc==1
    	            t_setup=0
        	        t_proc=0  
            	    timing_str=",#{t_setup},#{t_proc}"
	            else
    	            timing_str=""                
        	    end             
            	cxx_service_tuples.push("\tservices[#{scid}]=ServicePair( #{0},&#{@@prefix_SC}SBA::#{lib}::#{wrapper}#{timing_str} );" )      
	        end
		end
        return cxx_service_tuples
    end    
    
    
    def cxx_servicenodes(cfg)
        cxx_servicenode_constants = []
        # For compatibility, we need S_* and SC_*		
        # S_ is the Service Node Id
        for servicenode_name_str in cfg['System']['ServiceNodes'].keys
            node_id = cfg['System']['ServiceNodes'][servicenode_name_str][0]
            # This is a HACK! The node is named after the first service in the list!
            service_name_str=cfg['System']['ServiceNodes'][servicenode_name_str][1][0]
            cxx_servicenode_constants.push("const UINT S_#{service_name_str} = #{node_id};")
        end
        #  SC_ is the Service Class Id
        for sc_str in cfg['System']['Services'].keys
            scid = cfg['System']['Services'][sc_str][0]
            cxx_servicenode_constants.push( "const UINT SC_#{sc_str} = #{scid};")
        end   
        return cxx_servicenode_constants.join("\n")        
    end
            
   	def cxx_nservicenodes(cfg)
		cfg['System']['ServiceNodes'].length
	end

def cxx_opcodes(cfg)
    servicetypes= cfg['System']['Services']
    interfaces = cfg['System']['ServiceClasses']
    cxx_method_constants=[]
    for st_key in interfaces.keys    
        scid = servicetypes[st_key][0].to_i
        scid_field = scid  << FS_SCId        
        methodid=0
        for m_key in interfaces[st_key]
            cxx_method_constants.push("const UINT M_#{st_key}_#{m_key} = #{scid_field+methodid};")
            methodid+=1
        end 
    end 
    
    return cxx_method_constants.join("\n")
end  


# ====================================================================  

cxxh_file="#{@@dirpath}/#{@@prefix_SC}SystemConfiguration.h"

cxxh=File.open(cxxh_file,"w")
    
cxxh.puts '
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
    
*/

//==============================================================================
//
// System Configuration
//
// GENERATED from YAML configuration using create_Cxx_SystemConfiguration.rb
//
//==============================================================================

// 
'
cxxh.puts "
#ifndef _#{@@prefix_SC}SBA_SYSTEM_CONFIGURATION_H_
#define _#{@@prefix_SC}SBA_SYSTEM_CONFIGURATION_H_
"
if (@@sysc==0)
cxxh.puts '
#ifndef STATIC_ALLOC
#include <map>
#endif
'

cxxh.puts '#include "ServiceCoreLibraries/'+@@sclib+'.h"'

else
cxxh.puts '
#include <map>
#include "SC_SBA.h"
'
end
cxxh.puts "
using namespace std;

typedef unsigned int UINT;
// WV: FIXME: this should go elsewhere!
#define FP 0 // No floating-point support in ALU

namespace #{@@prefix_SC}SBA {

"
if (@@sysc==0)
    cxxh.puts cxx_opcodes(cfg)
    cxxh.puts cxx_servicenodes(cfg)
end
cxxh.puts '
// Not elegant, but static arrays are a lot faster than linked lists!'
    cxxh.puts "const UINT NSERVICES = #{cxx_nservicenodes(cfg)};"

cxxh.puts '
class Config {
	public:	
	Services services;
#ifndef NO_DRI	
	// FIXME: Configurations for Dynamic Reconfiguration/Loading should
	// not be statically defined but read from a file or command line
    Configurations configurations;
#endif // NO_DRI    
'
if @@sysc==1
  cxxh.puts '
  Timings timings;
  '
end
cxxh.puts '
	Config()
	{
'
if @@sysc==0
cxxh.puts '
    // for static allocation. By checking service_address we know if the slot is empty or not
#ifdef STATIC_ALLOC
    for (uint i=0;i<MAX_NSERVICES;i++) {
            services[i]=ServicePair(MAX_NSERVICES,&SBA::'+@@sclib+'::none);
    }
#ifndef NO_DRI    
    for (uint i=0;i<MAX_NDYNCONFIGS;i++) {
            services[i]=DynConfigPair("UNDEFINED_LIBRARY","UNDEFINED_SYMBOL");
    }    
#endif // NO_DRI    
#endif
'
else
cxxh.puts '
	services[0]= ServicePair(gw_address,&SC_SBA::SCLib::sba_GATEWAY);
'	
end	
#cxxh.puts '
#/*
# * It is crucial that the addresses (first elt of ServicePair) are contiguous
# * The service ids (indices of the services array) do not need to be (services is a map)
# * 
# * Currently, any id > 32 will be sent to the bridge
# * 
# */	
#
#// services[service_id]=ServicePair(service_address,&SBA::SCLib::ls_LET);
#'
cxxh.puts '#ifndef NO_SERVICES'
    cxxh.puts cxx_serviceclasses(cfg)

cxxh.puts '#endif // NO_SERVICES'
cxxh.puts '// NO DRI SUPPORT YET for NEW!'

if @@sysc==1
  cxxh.puts sysc_timings(cfg)
end
cxxh.puts '
    };    
	
};
'
cxxh.puts "		
} // #{@@prefix_SC}SBA
#endif /*_#{@@prefix_SC}SBA_SYSTEM_CONFIGURATION_H_*/
"

cxxh.close           

# FOR SYSTEMC ================================================================= 
if (@@sysc==1) 

sysc_consts_file="#{@@dirpath}/#{@@prefix_SC}SystemConfigurationConsts.h"
sysc_consts=File.open(sysc_consts_file,"w")
sysc_consts.puts '
#ifndef _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_
#define _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_


#include "SC_SBA.h"

using namespace std;

typedef unsigned int UINT;

namespace SC_SBA {
'
if @@has_let==0
sysc_consts.puts '    
const UINT S_LET = 3;
const UINT SC_LET = 0;    
'
end
sysc_consts.puts cxx_opcodes(cfg)

sysc_consts.puts '
} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/
'
sysc_consts.close
# END of FOR SYSTEMC ================================================================= 
end

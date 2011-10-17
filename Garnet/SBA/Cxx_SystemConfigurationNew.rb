# Cxx_SystemConfiguration.rb
#
# :title: Gannet Service-based SoC project - System Configuration module
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
#==============================================================================
#
# Service-based SoC project - C++ System Configuration Generator
#
#==============================================================================
#
# $Id: SystemConfiguration.rb 2446 2009-03-27 17:31:57Z socgroup $


#WV20110608: if you want the old behaviour back, revert to r4986
require 'yaml'
#require "SBA/Packet.rb"

if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
else    
    raise "Please define GANNET_DIR"
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
@@dirpath="./"
@@sba_dir="./"

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
    if File.exists?("#{SBA_WD}/Gannet/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{SBA_WD}/Gannet/#{lib}.yml"))
    elsif File.exists?("#{ENV['GANNET_DIR']}/SystemConfigurations/#{lib}.yml")
        libcfg=  YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{lib}.yml"))
    else
        raise "Can't find Library Config File #{lib}.yml"
    end
    return libcfg
end


#module SBA_SystemConfiguration

=begin
  We need to read the Application config file from . (and only from .)
  Then we lookup up the Library config files in ./Gannet and $GANNET_DIR/SystemConfigurations
  Application config has ServiceNodes and Aliases
  Library config has Services and ServiceClasses
=end

    appcfg =  YAML.load( File.open("#{SBA_YML}") )
    libs = appcfg['System']['Libraries']
    NServiceNodes=appcfg['System']['ServiceNodes'].keys.length
        
    i=0
    sclibs=[]
    libcfgs=[]
    for lib in libs
        libcfgs[i] =  loadLibraryConfig(lib)
                
        if File.exists?("#{SBA_WD}/Gannet/#{lib}.rb") or File.exists?("#{SBA_WD}/Gannet/#{lib}.h")
            sclibs[i]='#include "'+SBA_WD+'/Gannet/'+lib+'.h"'
        elsif File.exists?("#{ENV['GANNET_DIR']}/Garnet/SBA/ServiceCoreLibraries/#{lib}.rb") or 
            File.exists?("#{ENV['GANNET_DIR']}/Garnet/SBA/ServiceCoreLibraries/#{lib}.h")
            sclibs[i]='#include "SBA/ServiceCoreLibraries/'+lib+'.h"'
        else
            raise "Can't find Library File #{lib}.rb"
        end                            
        i+=1
    end
    
    def cxx_serviceclasses(appcfg,libcfgs)
        nthreads=1
        if @@sysc==1
            t_setup=0
            t_proc=0
            timing_str=",#{t_setup},#{t_proc}"
        else
            timing_str=""
        end

        cxx_service_tuples=[]
        for k in appcfg['System']['ServiceNodes'].keys
            entry=appcfg['System']['ServiceNodes'][k]
            node_id_str =entry[0]
            node_id = node_id_str.to_i            
            for scname in entry[1]                
                serviceclass=scname.split('.')
                sclid=1
                for libcfg in libcfgs
                    libn=libcfg['System']['Library']                        
                    if libn==serviceclass[0]
                        services= libcfg['System']['Services']
                        for sc_str in services.keys
                            if sc_str==serviceclass[1]
                                entry=services[sc_str]
                                service_id_str =entry[0]
                                scid = service_id_str.to_i            
                                core_method_name=entry[1]
                                sclid_scid=(sclid<<8)+scid
                                cxx_service_tuples.push("\tservices[#{sclid_scid}]=ServicePair( #{0},&#{@@prefix_SC}SBA::#{libn}::#{core_method_name}#{timing_str} );" )     
                                break
                            end
                        end
                        break
                    end
                    sclid+=1
                end
            end
        end
        return cxx_service_tuples.join("\n")
    end  

            
    def cxx_serviceclass_constants(libcfgs)
        serviceclass_constants=[]
        for cfg in libcfgs
            services=cfg['System']['Services']
            for sc_str in services.keys
                entry=services[sc_str]
                service_id_str =entry[0]
                scid = service_id_str.to_i            
                sc_lib=cfg['System']['Library']
                serviceclass_constants.push("const UINT SC_#{sc_lib}_#{sc_str} = #{scid};")                
            end
        end
        return serviceclass_constants.join("\n")
    end
    

#FIXME
# Configurations and HW info belong to the Library config
# So I need to loop over all libs here too
    def configurations(cfg)
        
        tConfigurations={}
        # TODO: see if this is still OK with the new format
        if cfg['System'].has_key?('Configurations')
            for conftype in cfg['System']['Configurations'].keys
                tConfigurations[conftype]={}
                for libid in cfg['System']['Configurations'][conftype].keys
                    entry=cfg['System']['Configurations'][conftype][libid]
                    tConfigurations[conftype][libid]=entry
                end
            end
        end
        return tConfigurations
    end
    
    # FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list
    # c3: [ 3, [LET] ]
    # This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]
    def servicenodes(appcfg)
        tServiceNodes={}
        for k in appcfg['System']['ServiceNodes'].keys
            entry=appcfg['System']['ServiceNodes'][k]
            node_id_str =entry[0]
            node_id = node_id_str.to_i
            if entry[1].length>1
                raise "Nodes with multiple service classes not yet supported"
            end
            service_class=entry[1][0]
            tServiceNodes[node_id]={}
            tServiceNodes[node_id]['Name']=service_class
            tServiceNodes[node_id]['Addr']=node_id
        end
        return tServiceNodes
    end        

    
    def cxx_method_constants(libcfgs)
        method_constants=[]
        sclid=1
        for cfg in libcfgs    
            sc_lib=cfg['System']['Library']
            for serviceclass in cfg['System']['ServiceClasses'].keys
                scid=cfg['System']['Services'][serviceclass][0].to_i
                scid_field = scid  << FS_SCId
                sclid_field = sclid  << FS_SCLId
                opcode=1 # The compiler starts opcodes at 1, so 0 is an error code            
                for methname in  cfg['System']['ServiceClasses'][serviceclass]
                    method_constants.push("const UINT M_#{sc_lib}_#{serviceclass}_#{methname} = #{sclid_field+scid_field+opcode};")
                    opcode+=1
                end
            end
            sclid+=1
        end
        return method_constants.join("\n")
    end
    
    def cxx_servicenode_constants(appcfg)
        
        servicenode_constants=[]
        for node_id in servicenodes(appcfg).keys
            servicenode_name_str = servicenodes(appcfg)[node_id]['Name']
            if servicenode_name_str=~/\.LET$/
                eval "S_LET = #{node_id}"
            end
            const_name_str=servicenode_name_str.sub('.','_') 
            servicenode_constants.push("const UINT S_#{const_name_str} = #{node_id};")
        end
        return servicenode_constants.join("\n")
    
    end # of cxx_servicenode_constants()




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
for libinclude in sclibs
    cxxh.puts libinclude
end

else
cxxh.puts '
#include <map>
#include "SC_SBA.h"
'
end
cxxh.puts "
using namespace std;

typedef unsigned int UINT;

namespace #{@@prefix_SC}SBA {

"
if (@@sysc==0)
    cxxh.puts cxx_method_constants(libcfgs)
    cxxh.puts cxx_servicenode_constants(appcfg)
    cxxh.puts cxx_serviceclass_constants(libcfgs)    
end
cxxh.puts '
// Not elegant, but static arrays are a lot faster than linked lists!'
    cxxh.puts "const UINT NSERVICES = #{NServiceNodes};"

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
    sclib ="NONE" #FIXME
cxxh.puts '
    // for static allocation. By checking service_address we know if the slot is empty or not
#ifdef STATIC_ALLOC
    for (uint i=0;i<MAX_NSERVICES;i++) {            
            services[i]=ServicePair(MAX_NSERVICES,&SBA::'+sclib+'::none);
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
    cxxh.puts cxx_serviceclasses(appcfg,libcfgs)

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
sysc_consts.puts cxx_method_constants(libcfgs)
sysc_consts.puts cxx_servicenode_constants(appcfg)
sysc_consts.puts cxx_serviceclass_constants(libcfgs)
sysc_consts.puts '
} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/
'
sysc_consts.close
# END of FOR SYSTEMC ================================================================= 
end

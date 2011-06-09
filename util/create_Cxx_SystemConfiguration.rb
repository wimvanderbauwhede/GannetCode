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
opts.on("-h","--help") {
puts help
exit
}
opts.parse(ARGV)

require 'yaml'
require "SBA/ServiceConfiguration.rb"

cxxh_file="#{@@dirpath}/#{@@prefix_SC}SystemConfiguration.h"
if (@@sysc==1)
    sysc_consts_file="#{@@dirpath}/#{@@prefix_SC}SystemConfigurationConsts.h"
end

if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
	cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))
else
	cfg = YAML.load(File.open(SBA_YML))
end

if cfg['System']['Version'].to_f >= 2.0
  NEW=1
else
  NEW=0
end 

if cfg['System']['Version'].to_f >= 2.1
  NEWER=1
else
  NEWER=0
end 


if NEWER==1
       @@sclib=cfg['System']['Library']
else
  @@sclib='SBAnew'
end
  
if NEW==1
require "SBA/SystemConfigurationNew.rb"
else
require "SBA/SystemConfiguration.rb"
end
  

# NEW CODE ====================================================================  
if NEW==1
    def cxx_serviceclasses(cfg)
     if NEWER==1
          require "SBA/ServiceCoreLibraries/#{@@sclib}.rb"        
          sclibclass=Object.const_get(@@sclib) #.new()
      end
      
        cxx_service_tuples=[] 
        for service_id_str in cfg['System']['Services'].keys
            scid_wrapper=cfg['System']['Services'][service_id_str] # BEGIN: [ 1, ls_BEGIN, 1 ]
            scid=scid_wrapper[0].to_i 
            wrapper=scid_wrapper[1]
#            puts "KERNEL: #{wrapper}"
            if SBAnew.method_defined?(:"#{wrapper}")
#              puts "#{@@sclib} has no method #{wrapper}"
              lsclib='SBAnew' #TODO: check the other classes              
            else
              lsclib=@@sclib
            end  
            # We must check somehow id a method exists in a given class
            # So we load the class and test ...
            if @@sysc==1
                t_setup=0
                t_proc=0  
                timing_str=",#{t_setup},#{t_proc}"
            else
                timing_str=""                
            end             
            cxx_service_tuples.push("\tservices[#{scid}]=ServicePair( #{0},&#{@@prefix_SC}SBA::#{lsclib}::#{wrapper}#{timing_str} );" )      
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
            
   	def cxx_nservices(cfg)
		cfg['System']['Services'].length
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


# END of NEW CODE ====================================================================  
    
else
    
# OLD CODE ====================================================================  
# Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads,[Tsetup,Tproc]], Addr: Service NoC Address }  

@@has_let=0

def cxx_services(cfg)
    gw_addr = cfg['System']['ServiceInstances'][0]['Addr'].to_i
    if gw_addr!=0
        raise "Gateway address MUST be 0!"
    end
    cxx_service_tuples=[] 
    for service_id_str in cfg['System']['ServiceInstances'].keys
        service_id =service_id_str.to_i
        ServiceInstances[service_id]={}
        noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
#        if noc_addr<gw_addr
        for service_name_str in cfg['System']['ServiceInstances'][service_id_str].keys            
            if service_name_str!='Addr'
                if service_name_str=='LET'
                    @@has_let=1
                end    
                entries=cfg['System']['ServiceInstances'][service_id_str][service_name_str]
                sctype=entries[0]
                scid=entries[1].to_i
                core_method_name=entries[2]
                nthreads=entries[3].to_i
                if @@sysc==1
                    t_setup=entries[4][0].to_i
                    t_proc=entries[4][1].to_i   
                    timing_str=",#{t_setup},#{t_proc}"
                else
                    timing_str=""                
                end             
                #ServiceInstances[service_id][scid]=[method,service_name_str,nthreads]
                ServiceInstances[service_id][scid]=[SBA_SCLib.method(:"#{core_method_name}"),service_name_str,nthreads]
                if (service_id!=0)                
                    cxx_service_tuples.push("\tservices[#{service_id}]=ServicePair( #{noc_addr},&#{@@prefix_SC}SBA::SCLib::#{core_method_name}#{timing_str} );" )      
                end                        
            else
                #noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
                ServiceInstances[service_id]['Addr']=noc_addr
            end                
        end
#        end
    end
    return cxx_service_tuples
end

def cxx_aliases(cfg)
    gw_addr = cfg['System']['ServiceInstances'][0]['Addr'].to_i
    if gw_addr!=0
        raise "Gateway address MUST be 0!"
    end        
    cxx_alias_tuples=[]
            
    for alias_str in cfg['System']['Aliases'].keys
        next if alias_str=='NONE'
        entries=cfg['System']['Aliases'][alias_str]
        service_name_str=entries[0]
        service_id_str=entries[1]
        noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
        service_id=service_id_str.to_i
        opcode=entries[2].to_i
        if @@sysc==1
            t_setup=entries[3][0].to_i
            t_proc=entries[3][1].to_i  "Level 1 labs" 
            timing_str=",#{t_setup},#{t_proc}"
        else
            timing_str=""                
        end         
#        if noc_addr<gw_addr
            cxx_alias_tuples.push("\t"+'aliases["'+ "#{alias_str}" + '"]=AliasPair("' + "#{service_name_str}" +'",' + "#{opcode}#{timing_str} );")            
#        end
    end
    return cxx_alias_tuples.join("\n") 
end

# configurations in C++ should actually be read at run time. But that's a detail
# We only care about 'native' for C++, so we have simply
#
# configurations[cfgid]=DynCfgPair(libstr,symbolstr);
# configurations can be a List<CfgTuple>
# class CfgTuple {
#string lib;
#string symbol; 
#}
def cxx_configurations(cfg)
    cxx_cfg_tuples=[]
    if (cfg['System'].has_key?('Configurations') and
    cfg['System']['Configurations'].has_key?('native') )
    for cfgid in cfg['System']['Configurations']['native'].keys 
    cfgpair=cfg['System']['Configurations']['native'][cfgid]           
      if @@sysc==1
        config_str=", #{cfgpair[2]}, #{cfgpair[3]}, #{cfgpair[4]}"
      else
        config_str=''
      end
            cxx_cfg_tuples.push("\t"+'configurations['+"#{cfgid}"+']=DynConfigTuple("'+cfgpair[0]+'","'+cfgpair[1]+'"'+config_str+');')            
    end
    end
    return cxx_cfg_tuples.join("\n") 
end

if @@sysc==1
  
def sysc_timings(cfg)
  gw_addr = cfg['System']['ServiceInstances'][0]['Addr'].to_i
    if gw_addr!=0
        raise "Gateway address MUST be 0!"
    end
      
  cxx_timings_tuples=[]
    servicetypes={}
  for service_id_str in cfg['System']['ServiceInstances'].keys
      service_id =service_id_str.to_i
      ServiceInstances[service_id]={}
      noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
#      if noc_addr<gw_addr
        for service_name_str in cfg['System']['ServiceInstances'][service_id_str].keys            
            if service_name_str!='Addr'   
                entries=cfg['System']['ServiceInstances'][service_id_str][service_name_str]
                    sctype=entries[0]
                    scid==entries[1].to_i
                t_setup=entries[3][0].to_i
                t_proc=entries[3][1].to_i   
                opcode=entries[0].to_i
                if (service_id!=0)                
                  cxx_timings_tuples.push("\t"+"timings[#{service_id}][#{opcode}]= TimingTuple(#{t_setup},#{t_proc});")    
                end                        
            end                
        end
#      end
  end    

  for alias_str in cfg['System']['Aliases'].keys
      next if alias_str=='NONE'
      entries=cfg['System']['Aliases'][alias_str]
      service_name_str=entries[0]
      service_id_str=entries[1]
      noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
      service_id=service_id_str.to_i
      opcode=entries[2].to_i # FIXME: should this not be combined with scid << FS_SCId ???
      t_setup=entries[3][0].to_i
      t_proc=entries[3][1].to_i      
#      if noc_addr<gw_addr
        cxx_timings_tuples.push("\t"+"timings[#{service_id}][#{opcode}]= TimingTuple(#{t_setup},#{t_proc});")            
#      end
  end
    if cfg['System'].has_key?('Services')
      interfaces={}
        interface_timings={}
        serviceids={}
            for servicetype in cfg['System']['Services'].keys
                interfaces[servicetype]={}
                methods = cfg['System']['Services'][servicetype]
                serviceids[servicetype]=methods['ServiceId']
                for methname in methods.keys
                    next if methname == 'ServiceId'
                    entry=methods[methname]
                    interfaces[servicetype][methname]=entry[0].to_i
                    interface_timings[servicetype][methname]=entry[1]
                end
            end  
           
      #FIXME: need to add timings for methods in SystemC as well
        for st_key in interfaces.keys    
            scid = serviceids[st_key]
            scid_field = scid  << FS_SCId
    
            for m_key in interface_timings[st_key].keys
                methid= interfaces[st_key][m_key]
                opcode=methid+scid_field
                entries= interface_timings[st_key][m_key]
                t_setup=entries[0].to_i
                t_proc=entries[1].to_i     
                # FIXME: only if noc_addr<gw_addr
                cxx_timings_tuples.push("\t"+"timings[#{service_id}][#{opcode}]= TimingTuple(#{t_setup},#{t_proc});") 
            end 
        end 
    end
  return cxx_timings_tuples.join("\n")   
end

end # of @@sysc

def cxx_constants(cfg)
    services=cfg['System']['ServiceInstances']
    cxx_service_core_constants=[]
    cxx_service_constants=[]
    cxx_alias_constants=[] 
    cxx_method_constants=[]
	service_names={}
    for service_id in services.keys
        for sc_name in services[service_id].keys
            next if sc_name=='Addr'             
            scid=services[service_id][sc_name][1]
            scid_field = scid << FS_SCId
            cxx_service_core_constants.push("const UINT SC_#{sc_name} = #{scid_field};")
            cxx_service_constants.push("const UINT S_#{sc_name} = #{service_id};")
			service_names[sc_name]=service_id
        end            
    end
	for control_service in ['LET','IF','APPLY','LAMBDA']
		if not service_names.has_key?(control_service)
			cxx_service_constants.push("const UINT S_#{control_service} = 0;")
		end
	end
    
    for key in aliases.keys
        next if key=='NONE' or key=='none'        
        service_id = aliases[key][1]        
        scid = aliases[key][0]
        scid_field = 0 # FIXME!!! scid  << FS_SCId
        key_lc=key.sub(/[:.]/,'_').downcase
        key_uc=key.sub(/[:.]/,'_').upcase
        cxx_alias_constants.push("const UINT A_#{key_lc} = #{scid_field+aliases[key][2]};")
        cxx_alias_constants.push("const UINT A_#{key_uc} = #{scid_field+aliases[key][2]};")
    end
    
    for st_key in interfaces.keys    
        scid = servicetypes[st_key]
        scid_field = 0 # FIXME!!! scid  << FS_SCId
        st_key_lc=st_key.sub(/[\.:]/,'_').downcase
        st_key_uc=st_key.sub(/[\.:]/,'_').upcase
        
        for m_key in interfaces[st_key].keys
            m_key_lc=m_key.downcase
            m_key_uc=m_key.upcase        
            cxx_method_constants.push("const UINT M_#{st_key}_#{m_key} = #{scid_field+interfaces[st_key][m_key]};")
            cxx_method_constants.push("const UINT M_#{st_key_uc}_#{m_key_uc} = #{scid_field+interfaces[st_key][m_key]};")
            cxx_method_constants.push("const UINT M_#{st_key_lc}_#{m_key_lc} = #{scid_field+interfaces[st_key][m_key]};")
            cxx_method_constants.push("const UINT A_#{st_key_uc}_#{m_key_uc} = #{scid_field+interfaces[st_key][m_key]};")
            cxx_method_constants.push("const UINT A_#{st_key_lc}_#{m_key_lc} = #{scid_field+interfaces[st_key][m_key]};")
        end 
    end 
    
    return cxx_service_core_constants.join("\n")+"\n\n"+cxx_service_constants.join("\n")+"\n\n"+cxx_alias_constants.join("\n")+"\n\n"+cxx_method_constants.join("\n")
end  
# END of OLD CODE ====================================================================  

end # of NEW  
    
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
if NEW==0
cxxh.puts '#include "ServiceCoreLibrary.h"'
else
  if NEWER!=1
    cxxh.puts '#include "ServiceCoreLibraryNew.h"'
  else
    cxxh.puts '#include "ServiceCoreLibraries/'+@@sclib+'.h"'
  end

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
// WV: FIXME: this should go elsewhere!
#define FP 0 // No floating-point support in ALU

namespace #{@@prefix_SC}SBA {

"
if (@@sysc==0)
if NEW==1
    cxxh.puts cxx_opcodes(cfg)
    cxxh.puts cxx_servicenodes(cfg)
else
	cxxh.puts cxx_constants(cfg)
end
end
cxxh.puts '
// Not elegant, but static arrays are a lot faster than linked lists!'
if NEW==1
    cxxh.puts "const UINT NSERVICES = #{cxx_nservices(cfg)};"
else 
    cxxh.puts "const UINT NSERVICES = #{cxx_services(cfg).length()};"
end

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
#ifndef NEW	
	unsigned int gw_address=0; // NSERVICES; // must be 0 from 16/12/2010 on (was the LAST address)
#endif	
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
if NEW==0	
cxxh.puts '	services[0]= ServicePair(gw_address,&SBA::SCLib::sba_GATEWAY);'
end
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
if NEW==1
    cxxh.puts cxx_serviceclasses(cfg)
else
    cxxh.puts cxx_services(cfg)
end

cxxh.puts '#endif // NO_SERVICES'
if NEW==0
cxxh.puts '#ifndef NO_DRI'
cxxh.puts cxx_configurations(cfg)
cxxh.puts '#endif // NO_DRI'
else
cxxh.puts '// NO DRI SUPPORT YET for NEW!'
end	
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
if NEW==1
    sysc_consts.puts cxx_opcodes(cfg)
else
    sysc_consts.puts 
end

sysc_consts.puts '
} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/
'
sysc_consts.close
# END of FOR SYSTEMC ================================================================= 
end

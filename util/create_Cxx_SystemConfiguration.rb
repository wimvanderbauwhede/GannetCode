#!/usr/bin/ruby 

#/** \file garnet.rb
#   
# \brief Service-based SoC project 
#
#*/
#
#/* ***** BEGIN LICENSE BLOCK *****
# * Version: AFL 2.1
# *
# * The contents of this file are subject to the Academic Free License Version
# * 2.1 (the "License"); you may not use this file except in compliance with
# * the License. You may obtain a copy of the License at
# * http://opensource.org/licenses/afl-2.1.php
# *
# * Software distributed under the License is distributed on an "AS IS" basis,
# * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# * for the specific language governing rights and limitations under the
# * License.
# *
# *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
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
    -Y, --yaml: generate YAML ServiceConfiguration.yml and exit
    -D, --dir: relative path to directory for generated files

EOH

require 'optparse'

opts=OptionParser.new()

@@sysc=0
@@prefix_SC=''
@@dirpath='./'
SBA_YML='SBA.yml'

opts.on("-S","--sysc") {@@sysc=1;@@prefix_SC='SC_'}
opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| SBA_YML=yml_file }
opts.on("-D dirpath","--dir=dirpath",String) {|dir_path| @@dirpath=dir_path }
opts.on("-h","--help") {
puts help
exit
}
opts.parse(ARGV)

NUM=1 # numerify
NUMK=1
WORDSZ=32 #64
TO_YAML=0
USE_THREADS=0
VERBOSE=0

require 'yaml'

require "SBA/ServiceConfiguration.rb"
require "SBA/SystemConfiguration.rb"
  
cxxh_file="#{@@dirpath}/#{@@prefix_SC}SystemConfiguration.h"
if (@@sysc==1)
    sysc_consts_file="#{@@dirpath}/#{@@prefix_SC}SystemConfigurationConsts.h"
end

if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
	cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))
else
	cfg = YAML.load(File.open(SBA_YML))
end

# Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads,[Tsetup,Tproc]], Addr: Service NoC Address }  

@@has_let=0

def cxx_services(cfg)
    gw_addr = cfg['System']['Services'][0]['Addr'].to_i
    cxx_service_tuples=[] 
    for service_id_str in cfg['System']['Services'].keys
        service_id =service_id_str.to_i
        Services[service_id]={}
        noc_addr=cfg['System']['Services'][service_id_str]['Addr']
        if noc_addr<gw_addr
        for service_name_str in cfg['System']['Services'][service_id_str].keys            
            if service_name_str!='Addr'
                if service_name_str=='LET'
                    @@has_let=1
                end    
                entries=cfg['System']['Services'][service_id_str][service_name_str]
                scid=entries[0].to_i
                core_method_name=entries[1]
                nthreads=entries[2].to_i
                if @@sysc==1
                    t_setup=entries[3][0].to_i
                    t_proc=entries[3][1].to_i   
                    timing_str=",#{t_setup},#{t_proc}"
                else
                    timing_str=""                
                end             
                #Services[service_id][scid]=[method,service_name_str,nthreads]
                Services[service_id][scid]=[SBA_SCLib.method(:"#{core_method_name}"),service_name_str,nthreads]
                if (service_id!=0)                
                    cxx_service_tuples.push("\tservices[#{service_id}]=ServicePair( #{noc_addr},&#{@@prefix_SC}SBA::SCLib::#{core_method_name}#{timing_str} );" )                    
                end                        
            else
                #noc_addr=cfg['System']['Services'][service_id_str]['Addr']
                Services[service_id]['Addr']=noc_addr
            end                
        end
        end
    end
    return cxx_service_tuples
end

def cxx_aliases(cfg)
    gw_addr = cfg['System']['Services'][0]['Addr'].to_i
    cxx_alias_tuples=[]
    for alias_str in cfg['System']['Aliases'].keys
        next if alias_str=='NONE'
        entries=cfg['System']['Aliases'][alias_str]
        service_name_str=entries[0]
        service_id_str=entries[1]
        noc_addr=cfg['System']['Services'][service_id_str]['Addr']
        service_id=service_id_str.to_i
        opcode=entries[2].to_i
        if @@sysc==1
            t_setup=entries[3][0].to_i
            t_proc=entries[3][1].to_i   
            timing_str=",#{t_setup},#{t_proc}"
        else
            timing_str=""                
        end         
        if noc_addr<gw_addr
            cxx_alias_tuples.push("\t"+'aliases["'+ "#{alias_str}" + '"]=AliasPair("' + "#{service_name_str}" +'",' + "#{opcode}#{timing_str} );")            
        end
    end
    return cxx_alias_tuples.join("\n") 
end

#def cxx_constants
#    cxx_service_constants=[]
#    cxx_alias_constants=[]    
#    for k in services.keys    
#        cxx_service_constants.push("const UINT S_#{services[k][2]} = #{k};")
#    end
#    for k in aliases.keys
#        cxx_alias_constants.push("const UINT A_#{k} = #{aliases[k][2]};")
#    end    
#    return cxx_service_constants.join("\n")+"\n\n"+cxx_alias_constants.join("\n")
#end


def cxx_constants(cfg)
    services=cfg['System']['Services']
    cxx_service_core_constants=[]
    cxx_service_constants=[]
    cxx_alias_constants=[] 
    for service_id in services.keys
        for sc_name in services[service_id].keys
            next if sc_name=='Addr'             
            scid=services[service_id][sc_name][0]
            cxx_service_core_constants.push("const UINT SC_#{sc_name} = #{scid << FS_SCId};")
            cxx_service_constants.push("const UINT S_#{sc_name} = #{service_id};")
        end            
    end
    
    for key in aliases.keys
        next if key=='NONE' or key=='none'        
        service_id = aliases[key][1]        
        scid = aliases[key][0]
        scid_field = scid  << FS_SCId
        key_=key.sub(/:/,'_')
        cxx_alias_constants.push("const UINT A_#{key_} = #{scid_field+aliases[key][2]};")
    end
    return cxx_service_core_constants.join("\n")+"\n\n"+cxx_service_constants.join("\n")+"\n\n"+cxx_alias_constants.join("\n")
end    
    
cxxh=File.open(cxxh_file,"w")
    
cxxh.puts '
/** \file SystemConfiguration.h
   
 \brief Gannet Service-based SoC project - C++/SystemC System Configuration
        
        Generated from SBA.yml with create_Cxx_SystemConfiguration.rb
*/

/* ***** BEGIN LICENSE BLOCK *****
 * Version: AFL 2.1
 *
 * The contents of this file are subject to the Academic Free License Version
 * 2.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://opensource.org/licenses/afl-2.1.php
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 *  
 *
 * ***** END LICENSE BLOCK ***** */

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
#include "ServiceCoreLibrary.h"
'
else
cxxh.puts '
#include <map>
#include "SC_sba.h"
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
cxxh.puts cxx_constants(cfg)
end
cxxh.puts '
//Not elegant, but static arrays are a lot faster than linked lists!'
cxxh.puts "const UINT NSERVICES = #{cxx_services(cfg).length()};"

cxxh.puts '
class Config {
	public:	
	Services services;

	Config()
	{
	unsigned int gw_address=NSERVICES; // must be the LAST address
'
if @@sysc==0
cxxh.puts '
    // for static allocation. By checking service_address we know if the slot is empty or not
#ifdef STATIC_ALLOC
    for (uint i=0;i<MAX_NSERVICES;i++) {
            services[i]=ServicePair(MAX_NSERVICES,&SBA::SCLib::none);
    }
#endif

	services[0]= ServicePair(gw_address,&SBA::SCLib::sba_GATEWAY);
'	
else
cxxh.puts '
	services[0]= ServicePair(gw_address,&SC_SBA::SCLib::sba_GATEWAY);
'	
end	
cxxh.puts '
/*
 * It is crucial that the addresses (first elt of ServicePair) are contiguous
 * The service ids (indices of the services array) do not need to be (services is a map)
 * 
 * Currently, any id > 32 will be sent to the bridge
 * 
 */	

// services[service_id]=ServicePair(service_address,&SBA::SCLib::ls_LET);
'
cxxh.puts '#ifndef NO_SERVICES'
cxxh.puts cxx_services(cfg)

cxxh.puts '#endif // NO_SERVICES
    };    
	
};
'
cxxh.puts "		
} // #{@@prefix_SC}SBA
#endif /*_#{@@prefix_SC}SBA_SYSTEM_CONFIGURATION_H_*/
"

cxxh.close           


if (@@sysc==1) 
sysc_consts=File.open(sysc_consts_file,"w")
sysc_consts.puts '
#ifndef _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_
#define _SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_


#include "SC_sba.h"

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
sysc_consts.puts cxx_constants(cfg)

sysc_consts.puts '
} // SC_SBA
#endif /*_SC_SBA_SYSTEM_CONFIGURATION_CONSTS_H_*/
'
sysc_consts.close
end

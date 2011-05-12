# SystemConfiguration.rb
#   
# :title: Gannet Service-based SoC project - System Configuration module
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
#==============================================================================
#
# Service-based SoC project - System Configuration module
#
#==============================================================================
#
# $Id: SystemConfiguration.rb 2446 2009-03-27 17:31:57Z socgroup $

require 'yaml'

require "SBA/Packet.rb"
if NEWER!=1
require "SBA/ServiceCoreLibraryNew.rb"
end
module SBA_SystemConfiguration
    
    if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
        cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))        
    else
        cfg = YAML.load(File.open(SBA_YML))
    end
    lib=cfg['System']['Library']
if NEWER==1
    require "SBA/ServiceCoreLibraries/#{lib}.rb"        
    sclib=Object.const_get(lib).new()
end

# What we need is a map scid => wrapper
# Then we simply call  @sba_system.services[scid].call()
    Services={}
    serviceclasses=  cfg['System']['Services']
    for sc_str in serviceclasses.keys
        entry=serviceclasses[sc_str]        
#		puts entry.inspect
        service_id_str =entry[0]
        scid = service_id_str.to_i
        core_method_name=entry[1]
        nthreads=1
if NEWER==1                
        Services[scid]=[sclib.method(:"#{core_method_name}"),sc_str,nthreads]
else                
        Services[scid]=[SBA_SCLib.method(:"#{core_method_name}"),sc_str,nthreads]                
end                
        

    end

# We also need a list of all opcodes, so we can make M_<Class>_<Method>
# So we need serviceclass=> {
#            'SCId' => Services[serviceclass][0],
#            'Methods'=> cfg['System']['ServiceClasses'][serviceclass]
#            }
    Interfaces={}
    for serviceclass in cfg['System']['ServiceClasses'].keys            
      puts serviceclass
      puts cfg['System']['Services'].inspect
        Interfaces[serviceclass]={}
        Interfaces[serviceclass]['Methods'] = cfg['System']['ServiceClasses'][serviceclass]
          puts "cfg['System']['Services'][#{serviceclass}]"
        Interfaces[serviceclass]['SCId'] = cfg['System']['Services'][serviceclass][0].to_i
    end  
    
    Configurations={}    
# TODO: see if this is still OK with the new format 
    if cfg['System'].has_key?('Configurations')
        for conftype in cfg['System']['Configurations'].keys
            Configurations[conftype]={}
            for libid in cfg['System']['Configurations'][conftype].keys
                entry=cfg['System']['Configurations'][conftype][libid]
                Configurations[conftype][libid]=entry
            end
        end  
    end        
    # FIXME: This is a hack for temporary compatibility. It assigns the node id to the first service class in the list 
    # c3: [ 3, [LET] ] 
    # This will break for multi-service nodes: c3: [3, Service1, LET, IF, ALU]    
    ServiceNodes={}
    for k in cfg['System']['ServiceNodes'].keys        
		entry=cfg['System']['ServiceNodes'][k]
		node_id_str =entry[0]
        node_id = node_id_str.to_i
        if entry[1].length>1
            raise "Nodes with multiple service classes not yet supported"
        end
        service_class=entry[1][0]        
        ServiceNodes[node_id]={}
        ServiceNodes[node_id]['Name']=service_class
        ServiceNodes[node_id]['Addr']=node_id
    end        
    Version = cfg['System']['Version'].to_i             
	NServices=cfg['System']['Services'].keys.length
	NServiceNodes=cfg['System']['ServiceNodes'].keys.length

    def servicenodes
        ServiceNodes
    end

    def serviceclasses
        Services
    end
    
    def services
        Services
    end
    
    def interfaces
        Interfaces
    end

    def configurations
        Configurations
    end

    def nservicenodes
        NServiceNodes
    end

    def nservices
        NServices
    end

	def version
		Version
	end

	def num2name(num)
#FIXME:WV10122009: num2name is BROKEN, 1 num can refer to several names!
# we simply take the first name, for now
            for name in ServiceNodes.keys
                if ServiceNodes[name]['Addr']==num			
                    return name
                end
			end	
			return "#{num}"
	end
end
include SBA_SystemConfiguration

# Actually "NSERVICENODES" would be a better name
	if version>=2.0
# New format
		NSERVICES=nservicenodes		
		for sc_key in interfaces.keys    
			scid = interfaces[sc_key]['SCId']
			scid_field = scid  << FS_SCId 
			opcode=0
			for methname in interfaces[sc_key]['Methods']                    
				eval "M_#{sc_key}_#{methname} = #{scid_field+opcode}" 
#				puts "M_#{sc_key}_#{methname} = #{scid_field+opcode}" 
				opcode+=1       
			end 
		end    
        # For compatibility, we need S_* and SC_*		
        for node_id in servicenodes.keys
            servicenode_name_str = servicenodes[node_id]['Name']
            eval "S_#{servicenode_name_str} = #{node_id}"
        end
        
        for scid in serviceclasses.keys
            sc_str = serviceclasses[scid][1]
            eval "SC_#{sc_str} = #{scid}"
        end     
        		
	else
# Old format
    end    
=begin

WV04122008: Multi-threaded cores provided by multiple service cores

Every service can have multiple aliases, and we can assign every alias to a particular thread
That means that the ServiceManager must have a (small) lookup table to know which tid can support
which alias. The K_S:Name provides the opcode, the lookup provides the tid. 

* The first complication is that we can have multiple tids per opcode
So knowing the opcode, the lookup would return a list of tids. On the other hand, we have the list
of free tids. What we need is rather a tid mask than an actual tid, 
i.e. a single bitmask should give the complete range

plus 0,1,2,3 => 00001111
minus
times
over 4 => 00010000
gt 5 => 00100000
lt
eq
not => 11000000

Then we take the table of tids and their status:

0 1
1 1
2 0
3 1
4 1
5 0
6 1
7 0

and we AND this with the mask. So for plus we get:

0 1
1 1

and we simply take the smallest tid

* The second complication is that a some opcodes can be supplied by different Service Cores than others
This means that we can, alas, not simply have

7 => [ls_ALU,19,'ALU'],

Suppost we have the following situation:

7 => [
        'ALU1': [ls_ALU1,17]
        'ALU2': [ls_ALU2,17]
     ]

This service has 4 opcodes plus,minus,times,over;
2 of these are provided by ls_ALU1, the other 2 by ls_ALU2

alu:plus (+)
alu:minus (-)
alu:times (*)
alu:over (/)

So we could do:

    plus: [ALU1, [0, 1, 2, 3], 9] # Every alias needs a number!
    minus: [ALU1, [0, 1, 2, 3], 10]
    times: [ALU2, [0, 1, 2, 3], 11]
    over: [ALU2, [4, 5], 12]

For the compiler, "+" would resolve to alu:plus, opcode 9; looking up 'alu:plus' gives ALU1, that gives 7
We now need to give ALU1 and ALU2 the list of tids on which they can run, so the YAML gets an extra field:

7 => [ls_ALU1,17,'ALU1',[0,1]]
7 => [ls_ALU2,17,'ALU2',[2,3]]

For simplicity, we could add a field for NCORE_THREADS, so we would have

7 => [SBA_SCLib.method(:ls_ALU1),17,'ALU1',4,[0,1]]

But of course we could simply ask for the length of the list (at least in Ruby)



Another use case is this:

42 => [cs_S42,42,'S42',[0,1,2,3]]
42 => [cs_IF,42,'S42_IF',[4,5]]
42 => [cs_ALU,42,'S42_ALU',[6,7]]

The code will contain:

S42 => will resolve to 42 as the service id; I think the opcode should actually be 0 or 1
'S42:return': [S42_IF,[4],0]
'S42:if': [S42_IF,[5],1]
'S42:plus': [S42_ALU,[6],0]
'S42:eq': [S42_ALU,[7],1]

(There is some redundancy here and some room for errors here, maybe we should not detail the tid for each alias?)

At compile time, S42:plus (might be 'S42+') will be looked up as an alias, it gets the opcode 0 

=> not right: how can we know the tid? so the K_S would now need the tid!

Threads/opcodes/aliases

- the default opcode is 0, the default thread is 0
- the SBA.yml entry is extended to include:
    - the tid
    - number of threads for that tid
    - maybe total number of threads? No
    - the order in the SBA.yml file determines the actual tid; so tid+nthreads is transformed into the mask
    
- the aliases entry contains the tid. I think at the moment number of threads is not controlled at level of aliases.
IN fact, the alias refers to the service name, so maybe the tid is not required.
- aliases have opcodes 

    3: [ls_LET, 6, LET, 0, 4]
    
- I think either we have no aliases and then the opcode is 0 or we do have aliases and then the opcode must be explicit.
In other words LET needs an entry in the Aliases with opcode 0

For multiple service core we get something like

    42: [cs_S42, 42, S42,0,2]
    42: [cs_IF, 42,S42_IF,1,1]
    42: [cs_ALU, 42,S42_ALU,2,1]
    
Aliases:
    'S42:if': [S42_IF, 1, 0]
    'S42:return': [S42_IF, 1, 1]
    'S42:plus': [S42_ALU, 2,0]
    'S42:lessthan': [S42_ALU, 2,1]


The K_S:Name field now gets the "tid" as an extra field. This is not the actual thread id but rather the id of the core providing the service. We limit NCORE_THREADS to 8 so 3 bits will do. Leaves 5 bits for the opcodes.
Note that now non-threaded, non-aliased services have a scid and opcode of 0. For non-threaded services, it is sufficient to take Name & F_Opcode (5 bits); the complement is (Name & F_SCId) >> FS_SCId

=end
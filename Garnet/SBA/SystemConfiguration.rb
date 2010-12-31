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
require "SBA/ServiceCoreLibrary.rb"

module SBA_SystemConfiguration
    #TODO: At the moment, service id numbering must be contiguous!
    # There's no reason to keep it like this -- in the long run :-)
    # Although contiguous network addresses do make sense!
    # key => method,network address, service identifier

=begin
ServiceConfig is the class that contains the config data read from SBA.yml
ServiceInstances is a list/map of ServiceConfig instances.
service_id => ServiceConfig.new({scid1=>[method,service_name_str,nthreads],scid2=>...,'Addr'=>})
=end

    class ServiceThreadConfig
        attr_reader :scid, :core_method_name,:name, :nthreads
        def initialize(service_name,thread_data)
            @core_method_name=thread_data[1]
            @scid=thread_data[0]
            @nthreads=thread_data[2]
            @name=service_name
        end
    end

    class ServiceConfig
        attr_reader :service_id,:address, :scids, :threads
        def initialize(service_id,service_data)
            @service_id=service_id
            @scids=[]
            @threads={}
           for service_name in service_data.keys
                if service_name == 'Addr'
                    @address=service_data['Addr']
                else
                    scid=service_data[service_name][0]
                    @scids.push(scid)
                    @threads[scid]=ServiceThreadConfig.new(service_name,service_data[service_name])
                end
            end
       end    
    end    
    
      
          
    if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
        cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))        
    else
        cfg = YAML.load(File.open(SBA_YML))
    end

# Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads], Addr: Service NoC Address, Service: Service Type }           
    ServiceInstances={}  
    ServiceTypes={}    
    for service_id_str in cfg['System']['ServiceInstances'].keys
        service_id =service_id_str.to_i
        ServiceInstances[service_id]={}
        for service_name_str in cfg['System']['ServiceInstances'][service_id_str].keys
#            puts    service_name_str         
            if service_name_str!='Addr'
                entries=cfg['System']['ServiceInstances'][service_id_str][service_name_str]
                sctype=entries[0]    
                scid=entries[1].to_i
                core_method_name=entries[2]
                nthreads=entries[3].to_i
                #Services[service_id][scid]=[method,service_name_str,nthreads]
                ServiceInstances[service_id][scid]=[SBA_SCLib.method(:"#{core_method_name}"),service_name_str,nthreads]
                ServiceTypes[sctype]=scid  # FIXME: possible conflict: inst1: [T1,0], inst2: [T2,1], inst3: [T2,0], inst4: [T1,1]      
            elsif service_name_str=='Addr'
                noc_addr=cfg['System']['ServiceInstances'][service_id_str]['Addr']
                ServiceInstances[service_id]['Addr']=noc_addr
            else
                # nothing 
            end                
        end
    end
    
    Aliases={}
    
#        'plus': [ALU, 7, 9] => ALU => scid
        
    for alias_str in cfg['System']['Aliases'].keys
#        puts alias_str
        next if alias_str=='NONE'
        entries=cfg['System']['Aliases'][alias_str]
        service_name_str=entries[0]
        service_id_str=entries[1]
        service_id=service_id_str.to_i
        opcode=entries[2].to_i
#        puts "#{service_id_str},#{service_name_str}"
        scid=cfg['System']['ServiceInstances'][service_id_str][service_name_str][1].to_i
        Aliases[alias_str]=[scid,service_id,opcode]
    end
          
    ALU_Names={}
    
    for alu_op in cfg['System']['ALU_Names'].keys
        entry=cfg['System']['ALU_Names'][alu_op]
        ALU_Names[alu_op]=entry
    end  

    Interfaces={}
    ServiceIds={}
    if cfg['System'].has_key?('Services')
        for servicetype in cfg['System']['Services'].keys            
            Interfaces[servicetype]={}
            methods = cfg['System']['Services'][servicetype]
            ServiceIds[servicetype]=methods['ServiceId']
            for methname in methods.keys
                next if methname == 'ServiceId'
                entry=methods[methname]
                Interfaces[servicetype][methname]=entry[0].to_i
            end
        end  
    end      
    
    Configurations={}
    
    if cfg['System'].has_key?('Configurations')
        for conftype in cfg['System']['Configurations'].keys
            Configurations[conftype]={}
            for libid in cfg['System']['Configurations'][conftype].keys
                entry=cfg['System']['Configurations'][conftype][libid]
                Configurations[conftype][libid]=entry
            end
        end  
    end        
    
    def services
        ServiceInstances
    end
    def servicetypes
        ServiceTypes
    end    
    def aliases
        Aliases
    end
    def interfaces
        Interfaces
    end
    def serviceids
        ServiceIds
    end     
    def alunames        
        ALU_Names
    end
    
    def configurations
        Configurations
    end
    
    
    def denum_alias(num)
        # Check aliases first
        for name in Aliases.keys
            if Aliases[name][2]==num
                return name
#                break
            end
        end
    end 
    
    
    def denum(num)    
        # Then check services
        for name in ServiceInstances.keys
            if ServiceInstances[name][2]==num
                return name
#                break
            end
        end                      
    end
    
    def nservices
        nservices_={}          
            for id in services.keys                      
                for scid in services[id].keys      
                    coderef=services[id][scid][0]
                    name=services[id][scid][1]
                    address=services[id]['Addr']
                    nservices_[name]=[coderef,address,id, scid]
                end
            end        
        return nservices_
    end
    
    def num2name(num)
#FIXME:WV10122009: num2name is BROKEN, 1 num can refer to several names!
# we simply take the first name, for now
        if ServiceInstances.has_key?(num)
            for scid in ServiceInstances[num].keys
                if scid != 'Addr'
                    return ServiceInstances[num][scid][1]
                end                
            end                
        else 
          warn "No NAME for #{num}"
          return "NOSERVICE"
        end
    end
   #skip
    def service_by_address(address)
        for service_id in ServiceInstances.keys
            if ServiceInstances[service_id]['Addr'] == address
                break
            end 
        end
        return service_id
    end
    def address_by_service(service)
        if ServiceInstances.has_key?(service)
            address=ServiceInstances[service]['Addr']       
#                            warn "SERVICE: #{service} ADDRESS: #{address}"
        else
            warn "No entry in ServiceInstances for #{service}"
            address=service  
        end 

        return address
    end
#endskip    
end
include SBA_SystemConfiguration

NSERVICES=services.keys.length-1 
#    addr_counter=0
    for service_id in services.keys
    #try auto-assigned addresses!
#    Services[key][1]=addr_counter 
#    addr_counter+=1
#        next if key=~/\W/
#        eval "S_#{services[key][2]} = #{key}"
        for scid in services[service_id].keys
            next if scid=='Addr'             
            service_name_str=services[service_id][scid][1]
#            puts "SC_#{service_name_str} = #{scid << FS_SCId}"
#            puts "S_#{service_name_str} = #{service_id}"
            eval "SC_#{service_name_str} = #{scid << FS_SCId}"
            eval "S_#{service_name_str} = #{service_id}"
        end            
    end
    
    for key in aliases.keys
#        next if key=~/\W/
        next if key=='NONE' or key=='none'        
        # new numbering scheme: alias=opcode, so we need corresponding service first
        service_id = aliases[key][1]        
        scid = aliases[key][0]
        scid_field = 0 # scid  << FS_SCId # FIXME: see below
#        key_=key.sub(/:/,'_')
         key_lc=key.sub(/[\.:]/,'_').downcase
         key_uc=key.sub(/[\.:]/,'_').upcase
#        puts "A_#{key_uc} = #{scid_field+aliases[key][2]}"
#        puts "A_#{key_lc} = #{scid_field+aliases[key][2]}"
        eval "A_#{key_uc} = #{scid_field+aliases[key][2]}"
        eval "A_#{key_lc} = #{scid_field+aliases[key][2]}"
    end

    for st_key in interfaces.keys    
        stid = serviceids[st_key]
        stid_field = 0 # stid  << FS_SCId # FIXME: We can't fit both the Service Core ID and the Service Type ID in the Name field
        # I think the Service Type ID is not required at run time so I set it to 0; 
        # We can't have the Service Core ID here as it makes no sense!
        st_key_lc=st_key.sub(/[\.:]/,'_').downcase
        st_key_uc=st_key.sub(/[\.:]/,'_').upcase
        
        for m_key in interfaces[st_key].keys            
            m_key_lc=m_key.downcase
            m_key_uc=m_key.upcase
            eval "M_#{st_key}_#{m_key} = #{scid_field+interfaces[st_key][m_key]}"        
            eval "M_#{st_key_uc}_#{m_key_uc} = #{scid_field+interfaces[st_key][m_key]}"
            eval "M_#{st_key_lc}_#{m_key_lc} = #{scid_field+interfaces[st_key][m_key]}"
            eval "A_#{st_key_uc}_#{m_key_uc} = #{scid_field+interfaces[st_key][m_key]}"
            eval "A_#{st_key_lc}_#{m_key_lc} = #{scid_field+interfaces[st_key][m_key]}"            
        end 
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
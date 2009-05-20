# SystemConfiguration.rb
#   
# :title: Gannet Service-based SoC project - System Configuration module
#
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#==============================================================================
#
# Service-based SoC project - System Configuration module
#
#==============================================================================
#
#// $Id: SystemConfiguration.rb 2446 2009-03-27 17:31:57Z socgroup $

require 'yaml'

require "SBA/Packet.rb"
require "SBA/ServiceCoreLibrary.rb"
module SBA_SystemConfiguration
    #TODO: At the moment, service id numbering must be contiguous!
    # There's no reason to keep it like this -- in the long run :-)
    # Although contiguous network addresses do make sense!
    # key => method,network address, service identifier

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

# service id => service core name, service address, service name, scid

=begin
ServiceConfig is the class that contains the config data read from SBA.yml
Services is a list/map of ServiceConfig instances.
service_id => ServiceConfig.new({scid1=>[method,service_name_str,nthreads],scid2=>...,'Addr'=>})
class ServiceConfig 
    attr_reader :address, scids, 
end

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

    Services={}    
    
    if ENV.has_key?('GANNET_DIR') and (SBA_YML=='SBA.yml')
#cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/Garnet-HW/SBA.yml"))
        cfg = YAML.load(File.open("#{ENV['GANNET_DIR']}/SystemConfigurations/#{SBA_YML}"))
    else
#cfg = YAML.load(File.open('SBA.yml'))
        cfg = YAML.load(File.open(SBA_YML))
    end

# Service Id: { Service Core name: [Service Core Id, Core Function name, Nthreads], Addr: Service NoC Address }   
   
#    for service_id_str in cfg['System']['Services'].keys
#        service_id =service_id_str.to_i        
#        entries=cfg['System']['Services'][service_id_str]
##        Services[service_id]=[SBA_SCLib.method(:"#{entries[2]}"),entries[4].to_i,entries[0],entries[1].to_i]
#    end      
   
    for service_id_str in cfg['System']['Services'].keys
        service_id =service_id_str.to_i
        Services[service_id]={}
        for service_name_str in cfg['System']['Services'][service_id_str].keys            
            if service_name_str!='Addr'
                entries=cfg['System']['Services'][service_id_str][service_name_str]
                scid=entries[0].to_i
                core_method_name=entries[1]
                nthreads=entries[2].to_i
                #Services[service_id][scid]=[method,service_name_str,nthreads]
                Services[service_id][scid]=[SBA_SCLib.method(:"#{core_method_name}"),service_name_str,nthreads]        
            else
                noc_addr=cfg['System']['Services'][service_id_str]['Addr']
                Services[service_id]['Addr']=noc_addr
            end                
        end
    end
    
    Aliases={}
    
#        'plus': [ALU, 7, 9] => ALU => scid
        
    for alias_str in cfg['System']['Aliases'].keys
        next if alias_str=='NONE'
        entries=cfg['System']['Aliases'][alias_str]
        service_name_str=entries[0]
        service_id_str=entries[1]
        service_id=service_id_str.to_i
        opcode=entries[2].to_i
        scid=cfg['System']['Services'][service_id_str][service_name_str][1].to_i
        Aliases[alias_str]=[scid,service_id,opcode]
    end
          
    ALU_Names={}
    
    for alu_op in cfg['System']['ALU_Names'].keys
        entry=cfg['System']['ALU_Names'][alu_op]
        ALU_Names[alu_op]=entry
    end  
    
    def services
        Services
    end
    def aliases
        Aliases
    end
    def alunames
        ALU_Names
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
        for name in Services.keys
            if Services[name][2]==num
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
        if Services.has_key?(num)
            for scid in Services[num].keys
                if scid != 'Addr'
                    return Services[num][scid][1]
                end                
            end                
        else 
          warn "No NAME for #{num}"
          return "NOSERVICE"
        end
    end
   #skip
    def service_by_address(address)
        for service_id in Services.keys
            if Services[service_id]['Addr'] == address
                break
            end 
        end
        return service_id
    end
    def address_by_service(service)
        if Services.has_key?(service)
            address=Services[service]['Addr']       
#                            warn "SERVICE: #{service} ADDRESS: #{address}"
        else
            warn "No entry in Services for #{service}"
            address=service  
        end 

        return address
    end
#endskip    
end
include SBA_SystemConfiguration

NSERVICES=services.keys.length-1 
    addr_counter=0
    for service_id in services.keys
    #try auto-assigned addresses!
#    Services[key][1]=addr_counter 
    addr_counter+=1
#        next if key=~/\W/
#        eval "S_#{services[key][2]} = #{key}"
        for scid in services[service_id].keys
            next if scid=='Addr'             
            service_name_str=services[service_id][scid][1]
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
        scid_field = scid  << FS_SCId
        key_=key.sub(/:/,'_')
        eval "A_#{key_} = #{scid_field+aliases[key][2]}"
    end


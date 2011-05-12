#!/usr/bin/ruby -w

#   
# :title: Gannet Service-based SoC project 
#
#--
#
# *
# *  (c) 2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#++

pid = fork do
if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
    gannet_dir="#{ENV['GANNET_DIR']}"
else
    raise "Please set GANNET_DIR=[path to your Gannet directory]"
end
#require 'Process'
#Process.daemon(nochdir=1,noclose=1)
# essential
DISTR=1
#NSERVICES=1

# config
WORDSZ=64
#VERBOSE=0
DEBUG_ALL=0

# useless
VM=1
TO_YAML=0
SEQVM=0
NEW=1
USE_THREADS=0
MEM=0



if ARGV.length!=4
puts ARGV.inspect
    raise "Please provide the node id, the YAML system config file and the multi-IP flag as arguments."
end
node_id=ARGV[0].to_i
multi_ip=ARGV[1].to_i
VERBOSE=ARGV[2].to_i
ymlfile=ARGV[3]
SBA_YML=File.exist?(ymlfile)?ymlfile:(File.exist?("#{gannet_dir}/SystemConfigurations/#{ymlfile}")?"#{gannet_dir}/SystemConfigurations/#{ymlfile}":raise("Can't find YAML file #{ymlfile}"))

require "SBA/ServiceConfiguration.rb"
if NEW==1
require "SBA/SystemConfigurationNew.rb"
else # NEW==0
require "SBA/SystemConfiguration.rb"
end # NEW
require "SBA/System.rb"

# a very thin wrapper around SBA_System for DISTR=1

include SBA_SystemConfiguration
servicenodes={}
servicenodes[node_id]={}
servicenodes[node_id]['Name']="#{node_id}"
servicenodes[node_id]['Addr']=node_id
            
@sba=SBA_System.new(servicenodes,configurations,[multi_ip])

require 'socket'
clnt=TCPSocket.new('127.0.0.1',7188)
clnt.puts("#{node_id}")
clnt.close()
puts "Node #{node_id} sent ping to barrier" if VERBOSE==1
@sba.run_proc()       


=begin
packets={}
require 'socket'
gw_port = 7188
port = gw_port + node_id
rxs = UDPSocket.new
txs=UDPSocket.new
rxs.bind(nil, port)
while (true)
    packet=[]
        text, sender = rxs.recvfrom(32) # accepting data in 16-byte chunks from the socket
        packet+=text.unpack("LLLL")
    puts "packet",packet.inspect
    if packet[0]==1
	# take action
  	    id=packet[3]
        puts "Packet ID:#{id}"
        task=[]
        if packets.has_key?(id)
	        task=packets[id]
        else
            # for debug
            task=[0,0,0,42]
        end
 	    dest=task[1]
        tid=task[3]
        tpacket=[1,dest,node_id,tid]
        tstr=tpacket.pack("CCCC")
        txs.send(tstr, 0, 'localhost', gw_port+dest)
    else # store packet
        packets[packet[3]]=packet
    end
    puts 'next'
end
=end

end
Process.detach(pid)

#!/usr/bin/ruby -w

=begin
The aim of this script is to start the gateway and the services as different processes.

The gateway runs on, well, the gateway, and uses the base port number
Every process takes as argument its port number. For "truly" distributed operation every
process will have a different IP address, obtained using the appropriate socket call.
But we must know in advance which process to run on which core.

So we have a flag multi_ip

	if (multi_ip) end else end

If there's only a single IP, the Node ID is added to the base port number
If there are multiple IPs, the Node ID is added to the base IP address
Consequently, we use the Node ID as an argument. And we always increment the base port number with the node id.

For multiple IPs, we will have to use ssh -f (or -n) host command args
For single IPs we simply have system('command args')

To know how many processes to start, and what the identifier would be, we need to parse the SystemConfiguration.

=end
if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
    @@gannet_dir="#{ENV['GANNET_DIR']}"
else
    raise "Please set GANNET_DIR=[path to your Gannet directory]"
end

# essential
DISTR=1
#NSERVICES=1

# config
WORDSZ=64
VERBOSE=1
DEBUG_ALL=1

# useless
VM=1
TO_YAML=0
SEQVM=0
NEW=1
USE_THREADS=0

require 'SBA/ServiceConfiguration'

# I guess what we need to do is open a listening Socket in a separate thread. 
# Then launch the node processes and let them talk to this socket. Once all nodes have sent a ping,
# then we close that socket and launch the gateway

def main()
    parse_opts()
    service_nodes=get_config()

    for k in service_nodes.keys
        v=service_nodes[k];
        node_id=v[0];
        launch_servicenode(node_id);
    end

    launch_gateway();
end

def get_config()

    if @@ymlfile=='SBA.yml'
        @@ymlfile=find_configpath(@@tdc_file)
    end
    puts @@ymlfile
    service_nodes={};

    require 'yaml'

    if File.exist?(@@ymlfile)
        config = YAML.load(File.open(@@ymlfile));
        service_nodes= config['System']['ServiceNodes']
    else
        raise "Configuration file #{@@ymlfile} does not exist."
    end
    return service_nodes
end
def parse_opts()

    require 'optparse'
    opts=OptionParser.new

    help=<<EOH
    Distributed GannnetVM launcher
    Outputs to STDOUT.
    0 [opts] .tdc-file
    -m: multiple IP addresses, for clusters. Default is single-IP.
    -v: verbose (for debugging)
    -d: dry-run (just print the commands);
    -I: Base IP address
    -P: Base port address
    -Y: YAML config file for Gannet (defaults to SBA.yml)    
    -R: Ruby processes   
EOH

    @@base_ip='127.0.0.1'
    opts.on("-I VAL","--ip=VAL",Integer) {|val| @@base_ip=val }
    @@base_port=7188
    opts.on("-P VAL","--port=VAL",Integer) {|val| @@base_port=val }
    @@verbose=0
    opts.on("-v","--verbose") {@@verbose=1;}
    @@rb=''
    opts.on("-R","--ruby") {@@rb='.rb';}
    @@multi_ip=0
    opts.on("-m","--multi") {@@multi_ip=1;}
    @@run=1
    opts.on("-d","--dryrun") {@@run=0;}

    @@ymlfile='SBA.yml'
    opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| @@ymlfile=yml_file }

    opts.on("-h","--help") {
        puts help
        exit
    }
    args=opts.parse(ARGV)
    if args.length==0
        raise "Please specify a .tdc file."
    end
    @@tdc_file=args.shift
end


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
    # If the bytecode contains the path to the YAML file, it should be in the
# remaining Words
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

def launch_servicenode(node_id)
    res=0;
    command='';
    if @@multi_ip==1
        node_ip=get_node_ip(node_id);
        # First need to scp YAML file to target node I guess?
        command="ssh -f #{node_ip} gannetnode#{@@rb} #{node_id} #{@@ymlfile} #{@@multi_ip}";
    else
        command="gannetnode#{@@rb} #{node_id} #{@@ymlfile} #{@@multi_ip} &";
    end
    puts command if @@verbose;
    if @@run==1
        res=system(command);
    end
    return res;
end

def launch_gateway()
    res=0;
    command='';
    command="gannetgw#{@@rb} #{@@tdc_file} #{@@multi_ip}";
    puts command if @@verbose;
    if @@run==1
        res=system(command);
    end
    return res;
end

def get_node_ip(node_id)
    bytes=@@base_ip.split('.')
    lastbyte=bytes[-1].to_i
    lastbyte+=node_id;
    bytes[-1]="#{lastbyte}"
    return bytes.join('.');
end

main()

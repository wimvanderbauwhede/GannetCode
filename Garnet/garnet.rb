#!/usr/bin/ruby 

# garnet.rb
#   
# :title: Gannet Service-based SoC project 
#
# (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
#
# ==============================================================================
#
# Service-based SoC project - Garnet, the Ruby Gannet
#
# ==============================================================================

# $Id: garnet.rb 2513 2009-04-15 13:47:03Z socgroup $

if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet-HW/")
else    
    $LOAD_PATH.push("#{ENV['HOME']}/SoC_Research/Code/Garnet-HW/")
end

help= <<EOH

    Usage: garnet [-hnSsqEQDBNCifRA] task description file
    -v, --verbose: Verbose :-)
    -n, --ncycles: Run for this number of clock cycles
    -s, --skipscheme: Don't run Scheme
    -S, --showtask: Show compiled task and exit"
    -q, --quit: Exit after parsing task description in Gateway
    -D, --debug: turn on debugging
    -M, --mem: turn on memory reporting
    -i, --int-only: Integer arithmetic only; default
    -f, --float-only: Floating-point
    -t, --threads: use Ruby threads
    -V, --vm: emulate Virtual Machine (default is to emulate HW)
    -R, --run-only: Run bytecompiled task description (default)
    -Y, --yaml: 
EOH


require 'optparse'
opts=OptionParser.new
ncycles=500
opts.on("-n VAL","--ncycles=VAL",Integer) {|val| ncycles=val }
skip_scheme=1 # was 0
opts.on("-s","--skipscheme") {skip_scheme=1 }
show_tasks=0
opts.on("-S","--showtasks") {show_tasks=1 }
VERBOSE=0
opts.on("-v","--verbose") {VERBOSE=1;}
# PRE=1
DATA=0 # no support for external DATA
USE_THREADS=0
opts.on("-t","--threads") {USE_THREADS=1;}
OLDVM=0
VM=0
opts.on("-V","--vm") {VM=1;}
EXT=1
NUM=1 # numerify
NUMK=1
NUML=1 # numeric labels
NUMP=0 # payload as list of numbers
BYC=1 # bytecode
SYM=1 # if 1, store all symbols/words as arrays of symbol/word
QUOTEFLD=1
FP=0 # FP or integer arithmetic 
QUIT=0
RUNONLY=1

RENAME=1 # renaming of refs etc in APPLY
DIRADDR=0 # direct addressing. Subsumed by address in subtask field
REDIR=1 # support for redirection (ACK, multicast)
NOT_SHARED=1 # all data have unique labels. required for UPDATE -- but breaks sine!
WORDSZ=32 #64
DEBUG_ALL=0
MEM=0
TO_YAML=0
IOMECH=0
opts.on("-E","--ext-syms") {EXT=1;} # obsolete
opts.on("-Q","--quote-as-field") {QUOTEFLD=1;} # obsolete
opts.on("-M","--mem") {MEM=1;}
opts.on("-D","--debug") {DEBUG_ALL=1;}
opts.on("-N","--numerify") {NUM=1;NUML=1;EXT=1; } # obsolete
opts.on("-B","--bytecompile") {BYC=1;NUM=1;NUML=1;EXT=1; warn "The -B option is obsolete"} # obsolete
opts.on("-C","--C-translatable") {BYC=1;NUM=1;NUML=1;NUMK=1;SYM=1;EXT=1;QUOTEFLD=1; } # obsolete
opts.on("-i","--int-only") {FP=0;}
opts.on("-f","--float-only") {FP=2;}
opts.on("-q","--quit") {QUIT=1;VERBOSE=1 }
opts.on("-R","--run-only") {BYC=1;NUM=1;NUML=1;NUMK=1;SYM=1;EXT=1;QUOTEFLD=1;RUNONLY=1; }
opts.on("-A","--direct-addressing") {DIRADDR=1; } # obsolete
opts.on("-y","--yaml") {TO_YAML=1;}
SBA_YML='SBA.yml'
opts.on("-Y yml-file","--yml=yml-file",String) {|yml_file| SBA_YML=yml_file }
opts.on("-h","--help") {
puts help
exit
}
# TODO: this allows only a single task description. Needs a loop around the compiler for multiple tasks.
td_file=opts.parse(ARGV).join('')
#td_file=(ARGV.length>0)?ARGV[0]:'NONE'
#ncycles=(ARGV.length>1)?ARGV[1].to_i():500
#skip_scheme=(ARGV.length>2)?ARGV[2].to_i():0
if td_file != 'NONE'
    IOMECH=1
    task_descriptions=[]
    if RUNONLY==0
        #Compile
        require "SBA/Compiler.rb"
        
        task_counter=1
        gannet_compiler=SBA_Compiler.new(td_file,task_counter) # FIXME: support multiple tasks! Task number can't be fixed at compile time!
        task_description=gannet_compiler.compile()
        task_data=gannet_compiler.data()
        task_descriptions.push([task_description,task_data])
    else # RUNONLY==1
        task_description=td_file
if DATA==1        
        task_data=td_file.sub(/\.tdc/,'.data');
        if not File.exist?(task_data)
            datah=File.open(task_data,"w")
            datah.print 0,"\n"
            datah.close
        end
        task_descriptions.push([task_description,task_data])
else # DATA==0
        task_descriptions.push([task_description])
end # DATA        
    end # RUNONLY
    #Run
    require "SBA/Runtime.rb"    
    garnet = SBA_Runtime.new(task_descriptions)	
    #	garnet.compile(td_file)
    if show_tasks==1
        garnet.show()
        exit
    end
    res=garnet.run(ncycles)
#    puts res.inspect
    #Run Scheme
    if skip_scheme==0
        #	garnet.scheme()
        print "RESULT Scheme:\t"
	if RUNONLY==1
		td_file.sub!(/\.tdc/,'.td')
	end
        puts `perl ./td2scm.pl #{td_file}`
    end
else 
    if TO_YAML==1
        require "SBA/Runtime.rb"
    else
        puts "Please specify a task description file"
    end    
end

""" \file SConstruct.test.py
   
 \brief Gannet SBA - SCons script for building CxxTest testsuite and runner
"""

## (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
    
# $Id$

#SetOption('implicit_cache', 1)

from GannetBuilder import build

classname = ''
binname=''
destdir='../../HardwareModel/SystemC/scsrc/SBA/'
csources=Split("""
ServiceConfiguration.cc
Bytecode.cc
Types.cc
Packet.cc
Memory.cc
TaskDescription.cc
LookupTable.cc
""")

sources=[]

for csource in csources:
    source=destdir+csource
    sources.append(source)

build(classname,binname,sources)

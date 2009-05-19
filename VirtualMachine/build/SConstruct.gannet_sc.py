""" \file SConstruct.test.py
   
 \brief Gannet SBA - SCons script for building CxxTest testsuite and runner
"""

## ##### BEGIN LICENSE BLOCK #####
 # Version: AFL 2.1
 #
 # The contents of this file are subject to the Academic Free License Version
 # 2.1 (the "License"); you may not use this file except in compliance with
 # the License. You may obtain a copy of the License at
 # http:#opensource.org/licenses/afl-2.1.php
 #
 # Software distributed under the License is distributed on an "AS IS" basis,
 # WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 # for the specific language governing rights and limitations under the
 # License.
 #
 #  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 #  
 #
 # ##### END LICENSE BLOCK ##### ##

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

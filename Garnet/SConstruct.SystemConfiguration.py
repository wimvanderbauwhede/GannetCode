""" \file SConstruct
   
 \brief Gannet SBA - SCons script for building SystemConfiguration for C++ GannetVM & SystemC
"""

# $Id$

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

ymlpath=ARGUMENTS.get('Y','../SystemConfigurations/SBA.yml')
lib_path=ARGUMENTS.get('D','.')
sysc=ARGUMENTS.get('sysc',0)
wordsz=ARGUMENTS.get('wordsz',32)
distr=ARGUMENTS.get('distr',0)

sc_prefix=''
sc_flag=''
if sysc=='1':
    sc_prefix='SC_'
    sc_flag='-S'

flags=''
if distr==1:
    flags='-P'

src_file='SBA/SystemConfiguration.rb'
target_file = lib_path+'/'+sc_prefix+'SystemConfiguration.h'
script='../util/create_Cxx_SystemConfiguration.rb'

env = Environment()

cmd='ruby '+script+' '+sc_flag+' '+flags+' -Y '+ymlpath+' -D '+lib_path+' -W '+str(wordsz)
gen=env.Command(target_file, src_file, cmd)
env.Alias('gen',target_file)
env.Depends(gen,script)

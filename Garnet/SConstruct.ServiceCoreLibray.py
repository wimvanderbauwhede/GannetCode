""" \file SConstruct
   
 \brief Gannet SBA - SCons script for building ServiceCoreLibrary for C++ GannetVM & SystemC
"""

# $Id$

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

ymlpath=ARGUMENTS.get('Y','SBA.yml')
lib_path=ARGUMENTS.get('D','.')
sysc=ARGUMENTS.get('sysc',0)

sc_prefix=''
sc_flag=''
if sysc=='1':
    sc_prefix='SC_'
    sc_flag='-S'

src_file = 'SBA/ServiceCoreLibrary.rb'
script='../util/r2n.pl'

env = Environment()

for cc_h in ['cc', 'h']:
    CC_H=cc_h.upper()
    target_file = lib_path+'/'+sc_prefix+'ServiceCoreLibrary.'+cc_h
    cmd='perl -I../util/ '+script+' -'+CC_H+' -S -Y '+ymlpath+' '+src_file+' > '+target_file
    gen=env.Command(target_file, src_file, cmd)
    env.Alias('gen',target_file)
    env.Depends(gen,script)
    

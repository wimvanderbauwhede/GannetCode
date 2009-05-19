""" \file SConstruct
   
 \brief Gannet SBA - SCons script for generating core source files for SystemC
"""

# $Id$

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

ymlpath=ARGUMENTS.get('Y','SBA.yml')
lib_path=ARGUMENTS.get('D','.')

sc_prefix='SC_'

script='../util/r2n.pl'

sources=Split("""
ServiceManager
Gateway
""")

env_scgen = Environment()
#Export('env_scgen')
#SConscript(lib_path+'/SConscript')

for source in sources:	
    print source
    src_file = 'SBA/'+source+'.rb'
    target_file = lib_path+'/'+sc_prefix+source+'.cc'
    cmd='perl -I../util '+script+' -CC -S -Y '+ymlpath+' '+src_file+' > '+target_file
    print cmd
    gen=env_scgen.Command(target_file, src_file, cmd)
    env_scgen.Alias('gen',target_file)
    env_scgen.Depends(gen,script)
    

'''\file SConstruct.test.py

    \brief Gannet SBA - SCons script for building Gannet

'''

##   (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>

# $Id$


###############################################################################
# DON'T MODIFY ANYTHING BELOW THIS
###############################################################################

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

from SCons.Variables import Variables
from SCons.Environment import Environment
#from SCons.Help import Help

from cxxtestgenlib import createRunner


def build(classname,binname,sources):

    destdir='../SBA/'
    global opts

    flags=[]
    switches=[]
    boost=0

    GEN=1
        # MACROS
    WORDSZ='WORDSZ=32'
    GL='GANNET_LANGUAGE'
    NEW='NEW=1'
    V='' #V='VERBOSE'
    # No SystemC by default
    SYSC=''
    SC_IDP=''
    SYSC_FIXME=''
    # Compile for VM (otherwise compiles to model HW)
    VM='VM=0'
#    OLDVM='OLDVM=1'
    USE_THREADS='USE_THREADS=0'
    use_pthreads = False
    THREADED_CORE='' # 'THREADED_CORE=0'
    threaded_core = False
    # Count CPU cycles
    CYCLES='' # 'CYCLES'
    TIMINGS='' # 'TIMINGS'
    STATIC_ALLOC='STATIC_ALLOC'

    # Flags
    WARN='-Wall '
    OPTSPEED    = '-O3 -fno-exceptions -fno-rtti '
    OPTSIZE = '-Os -fno-exceptions -fno-rtti '
    OPTTHREADS = '-O3 '
    OPTSYSC =  '-O3 '
    DEBUG = ''
    OPT = OPTSYSC
    CYGWIN=0

    # These are used by the build script to generate flags/switches
    GUI=None
    OSX=0
    # Flag for cross-compilation
    XC=0
    # Use LLVM
    # LLVM=1: x86, LLVM=2: ppc
    LLVM=0
    # SystemC
    SC=0
    H=0 # Help
    MACROS = ['INTERFACE_OBJ']
    yaml_config='../../SystemConfigurations/SBA.yml'

    #use options without leading '-': scons v=0 gui=QtGui
    opts = Variables()
    opts.Add('v', 'Verbose', 0)
    opts.Add('gui', 'Use Qt GUI', "QtGui")
    opts.Add('xc', 'Crosscompile',0)
    opts.Add('llvm', 'Use LLVM',0)
    opts.Add('sysc','Use SystemC',0)
    opts.Add('win','CygWin',0)
    opts.Add('vm', 'Virtual Machine',0)
# options can't take . or / in the strings!!
#    opts.Add('yml','YAML configuration file','') #'../../SBA.yml')
    opts.Add('cycles', 'Count CPU cycles',0)
    opts.Add('timings', 'Time program execution',0)
    opts.Add('dyn', 'Dynamic memory',0)
    opts.Add('pthreads', 'Use POSIX Threads',0)
    opts.Add('ptcore', 'Use POSIX Threaded Core',0)
    opts.Add('dbg', 'Debug',0)
    opts.Add('nogen',"Don't generate C++ sources from Ruby code",0) 
    opts.Add('opt', 'Optimise','speed') # or 'size'
    opts.Add('D','Macros (add as a string: D="MACRO1:1 MACRO2 MACRO3:whatever"\nSCons is too stupid to allow "=")','') # add additional macros as a string
    opts.Add('h', 'Help',0)

    args=sys.argv[1:]
    #import getopt
    #rest = getopt.getopt(args,"hABCD")
    for arg in args:
        if re.match("(\w+)=(\w+)",arg):
            (k,v)=arg.split('=')
            opts.args[k]=v

    #exit(opts.options)

    for param in os.environ.keys():
        if param == "VERBOSE":
            V='VERBOSE'
        if param == "GANNET_YML_CONFIG":
            yaml_config=os.environ["GANNET_YML_CONFIG"]

    for option in opts.options:
        if option.key == 'nogen' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            GEN=0
        if option.key == 'v' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            V='VERBOSE'
        if option.key == 'gui' and opts.args.has_key(option.key) and opts.args[option.key]==option.default:
            GUI='QtGui'
        if option.key == 'xc' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            XC=1
            OPT=OPTSPEED
        if option.key == 'llvm' and opts.args.has_key(option.key): # and opts.args[option.key]!=option.default:
            if opts.args[option.key]=='1':
                LLVM=1
            elif opts.args[option.key]=='2':
                LLVM=2
            else:
                LLVM=0
            OPT=OPTSPEED
        if option.key == 'sysc' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            SC=1
            SYSC='SYSC'
            SC_IDP='SC_INCLUDE_DYNAMIC_PROCESSES'
            SYSC_FIXME='SYSC_FIXME=1'
            STATIC_ALLOC=''
            destdir='../../HardwareModel/SystemC/scsrc/SBA/'
        if option.key == 'win' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYGWIN=1
        if option.key == 'vm' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VM='VM=1'
# doesn't work if the path has dots or slashes!
#        if option.key == 'yml' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
#            print "YAML!"
#            yaml_config=opts.args[option.key]
        if option.key == 'pthreads' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            USE_THREADS='USE_THREADS=1'
            use_pthreads=True
            OPT=OPTTHREADS
        if option.key == 'ptcore' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            THREADED_CORE='THREADED_CORE=1'
            threaded_core=True
            OPT=OPTTHREADS
        if option.key == 'cycles' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYCLES='CYCLES'
        if option.key == 'timings' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            TIMINGS='TIMINGS'
        if option.key == 'dyn' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            STATIC_ALLOC=''
        if option.key == 'dbg' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            DEBUG='-g ' #'-g -fno-exceptions -fno-rtti '
        if option.key == 'opt' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            if SC!=1:
                OPT=OPTSYSC #IZE
            else:
                OPT=OPTSYSC
        if option.key == 'D' and  opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            macrostr=re.sub('\s*:\s*','=',opts.args[option.key])
            MACROS=macrostr.split(' ')
        if option.key == 'h' and opts.args.has_key(option.key):
            H=1

    if commands.getoutput("uname") == "Darwin":
        OSX=1
        switches+=['DARWIN']

    if XC==1:
        switches.append('__ppc__')


    FLAGS=''
    SWITCHES=''
    flags+=[WARN,DEBUG,OPT]
    switches+=[SYSC,SC_IDP,SYSC_FIXME,V,VM,WORDSZ,CYCLES,TIMINGS,STATIC_ALLOC,USE_THREADS,THREADED_CORE]+MACROS
    for flag in flags:
        if flag !='':
            FLAGS+=flag+' '

    for switch in switches:
        if re.search('BOOST',switch):
            boost=1
        if switch != '':
            SWITCHES+='-D'+switch+' '

    if classname!='':
        csources=sources+[destdir+classname+'.cc']
        if os.path.exists('../../Garnet/SBA/'+classname+'.rb'):
            sources.append(destdir+classname+'.cc')
        if os.path.exists('test_'+classname+'.h'):
            sources.append('test_'+classname+'.cc')
    else:
        csources=sources

    if binname=='':
        bin='test_'+classname+'_runner'
    else:
        bin=binname
        sources.append(binname+'.cc')

    #------------------------------------------------------------------------------

    cxx='g++'
    if XC==1:
        cxx='powerpc-405-linux-gnu-g++'
    if LLVM==1:
        cxx='i686-pc-linux-gnu-g++'

#    if SC==1 and CYGWIN==0:
#        cxx='g++-3.3.6'

    env = Environment(variables = opts, CXX = cxx, CXXFLAGS = FLAGS+SWITCHES)
#   Help(opts.GenerateHelpText(env))

    if H==1:
        print(opts.GenerateHelpText(env))
        exit(1)

    if XC==1:
        HOME=os.environ['HOME']
        env['ENV']['PATH']=os.environ["GANNET_XC_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']

    if LLVM==1:
        env['ENV']['PATH']=os.environ["GANNET_LLVM_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']

    if GEN==1:
        envr2n = Environment()
        r2n=['../../util/r2n.pl']
    
        targetscc=[]
        targetsh=[]
    
        for csource in csources:
            if not re.search('test_|LookupTable|Types|SystemConfiguration|gannet|Socket|Timings|cs_',csource):
                tsource=re.sub(destdir,'../../Garnet/SBA/',csource)
                source=re.sub('\.cc','.rb',tsource)
                target=re.sub('^.*\/','',csource)
                target=destdir+re.sub('\.cc','',target)
                if STATIC_ALLOC!='':
                    targetcc=envr2n.Command(target+'.cc',source,"perl -I../../util ../../util/r2n.pl -Y "+yaml_config+" -s -CC $SOURCE > $TARGET")
                else:
                    targetcc=envr2n.Command(target+'.cc',source,"perl -I../../util ../../util/r2n.pl -Y "+yaml_config+" -CC $SOURCE > $TARGET")
                targetscc.append(targetcc)
                targetsh.append(envr2n.Command(target+'.h',source,"perl -I../../util ../../util/r2n.pl -Y "+yaml_config+" -H $SOURCE > $TARGET"))
#        if re.search('SystemConfiguration',csource):
#            targetsh.append(envr2n.Command(target+'.h',source,'/usr/bin/ruby -I ../../ ../../create_Cxx_SystemConfiguration.rb && cp -f SystemConfiguration.h ../SBA'))

    runner = 'ErrorPrinter'

    def sconsWrapper(target,source,env):
        #converting from list of Node to list of string
        filenames=[]
        for item in source:
            filenames.append(str(item))
        createRunner(filenames,str(target[0]),runner,GUI,SC) # call createRunner from cxxtestgenlib
        return None

    if binname=='' and SC!=1:
        outputFileName = 'test_'+classname+'.cc'
        files = ['test_'+classname+'.h']
        env.Command(outputFileName,files,sconsWrapper)

    libs=['m']
    if use_pthreads or threaded_core:
        libs+=['pthread']
#libs+=['pthread','boost_thread-mt']

    # SBA classes should not use boost shared libraries, only header files
    if binname!='' and boost==1:
        libs+=['boost_program_options']

    INCpaths=['.']
    LIBpaths=[]

    if boost==1:
        INCpaths+=[os.environ['BOOST_INC']]

    if SC==1 and not OSX and not CYGWIN:
        INCpaths+=[os.environ['SYSTEMC_INC'],os.environ['SYSTEMC_TLM']]
        LIBpaths+=[os.environ['SYSTEMC_LIB']]

    if OSX==1:
        INCpaths=['.']
        LIBpaths=['/usr/local/lib/']
        libs=['m']
        if boost==1:
            INCpaths+=[os.environ['BOOST_INC']]
            libs+=[os.environ['BOOST_LIB']] 
        
    if SC==1 and OSX==1:
        INCpaths+=[os.environ['SYSTEMC_INC'],os.environ['SYSTEMC_TLM']]
        LIBpaths+=[os.environ['SYSTEMC_LIB']]
#        INCpaths+=['/opt/systemc-2.2/include/','/opt/systemc-2.2/include/tlm'] #FIXME!
#        LIBpaths+=['/opt/systemc-2.2/lib-macosx/'] #FIXME!
#           INCpaths+=['/usr/local/systemc-2.1/include/']
#           LIBpaths+=['/usr/local/systemc-2.1/lib-macosx/']

    if SC==1 and CYGWIN==1:
        INCpaths+=[os.environ['SYSTEMC_INC'],os.environ['SYSTEMC_TLM']]
        LIBpaths+=[os.environ['SYSTEMC_LIB']]
#INCpaths+=['/cygdrive/c/Waqar/DCS/SystemC/systemc-2.1.v1/include','/cygdrive/c/Waqar/DCS/SystemC/TLM-2005-04-08/tlm'] #FIXME!
#        LIBpaths+=['/cygdrive/c/Waqar/DCS/SystemC/systemc-2.1.v1/lib-cygwin'] #FIXME!

    if SC==1:
        libs+=['systemc']

    if GUI:
        INCpaths+=['/usr/lib/qt3/include/'] #FIXME!
        listL=commands.getoutput("pkg-config --libs-only-L qt-mt").split("-L")
        listL.remove('')
        LIBpaths+=listL
        listl=commands.getoutput("pkg-config --libs-only-l qt-mt").split("-l")
        listl.remove('')
        libs+=listl
    #WV: to have multiple targets, we just need to set bin : bin is short for
    #env.Program(target=bin,...)
    if SC==1:
		sclib=env.Library('gannet',sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
		env.Install('../../HardwareModel/SystemC/lib',sclib)
		env.Alias('movelib','../../HardwareModel/SystemC/lib')
		env.Depends(sclib,targetscc+targetsh+r2n)
    else:
        prog=env.Program(bin,sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
        env.Install('../../bin',prog)
        env.Alias('install','../../bin')
        if GEN==1:
			env.Depends(prog,targetscc+targetsh+r2n)



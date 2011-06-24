import yaml
import getopt, sys
import os


def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "Y:", [ "yml="])
    except getopt.GetoptError, err:
        print str(err) # will print something like "option -a not recognized"
        sys.exit(2)
    for o, arg in opts:
        if o == '-Y':
            ymlpath=arg
    print ymlpath
    libsrcpaths = getLibSourcePaths(ymlpath)
    print libsrcpaths

def getLibSourcePaths(appcfg):    
    ymlh = open(appcfg, 'r')
    ymldata=ymlh.read()
    ymlh.close()
    cfg=yaml.load(ymldata)
    libs = cfg['System']['Libraries']
# actual libs are either in ./Gannet, $GANNET_DIR/Garnet/SBA/ServiceCoreLibraries or  $GANNET_DIR/VirtualMachine/SBA/ServiceCoreLibraries
# and for the moment they can be either Ruby or C++
    libsrcpaths=findPaths(libs)
    return libsrcpaths
 
def findPaths(libs):
    GANNET_DIR=os.environ['GANNET_DIR']
    print GANNET_DIR
    sclibs=[]
    for lib in libs:
        for path in [
            ["./Gannet/",".cc"],
            [GANNET_DIR+'/Garnet/SBA/ServiceCoreLibraries/','.cc'],
            ["./Gannet/",".rb"],
            [GANNET_DIR+'/Garnet/SBA/ServiceCoreLibraries/','.rb']
        ]:
            fullpath=path[0]+lib+path[1]
            print fullpath
            if os.path.exists(fullpath):
                sclibs.append(fullpath)
                break
    return sclibs                                  

if __name__ == "__main__":
    main()
    

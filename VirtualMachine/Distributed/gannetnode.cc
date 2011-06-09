/*
 *  (c) 2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */


#include "../SBA/System.h"
#include "../SBA/ServiceConfiguration.h"
#include "../SBA/SystemConfigurationNew.h"

#include "TCPSocket.h"

#define DISTR 1
// config
#define WORDSZ 64
#define DEBUG_ALL 0

// useless
#define VM 1
#define TO_YAML 0
#define SEQVM 0
#define NEW 1
#define USE_THREADS 0
#define MEM 0


int main(int ac, char* av[]) {

    unsigned int node_id=atoi(av[1]);
    unsigned int multi_ip=atoi(av[2]);
    int pid;

    int pid = fork() 
    if (pid<0) exit(1);
    if (pid>0) exit(0);
    
//  chdir("/");
//  setsid() /* obtain a new process group */
//	umask(0);

    SBA::System sba(node_id,base_ip);

    TCPSocket clnt(ip_addr,port);
    clnt.send(node_id);
    clnt.close()
#if VERBOSE==1
    cout << "Node "<<node_id<<" sent ping to barrier";
#endif
    sba.run_proc()       
    return 0;
}


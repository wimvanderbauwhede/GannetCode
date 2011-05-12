/*
 *  (c) 2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */


#include "../SBA/Node.h"

int main(int ac, char* av[]) {

		unsigned int node_id=atoi(av[1]);
		bool multi_ip=(atoi(av[2])==1);

		SBA::Node gannetnode(node_id,multi_ip);
        gannetnode.run();
        return 0;
}

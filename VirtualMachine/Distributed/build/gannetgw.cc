/*
 *  (c) 2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */


#include "../SBA/GatewayNode.h"
#include <string>

int main(int ac, char* av[]) {
	string tdc_file=av[1];
	bool multi_ip=(atoi(av[2])==1);

	SBA::GatewayNode gannetgw(tdc_file,multi_ip);
	gannetgw.run();
	return 0;
}

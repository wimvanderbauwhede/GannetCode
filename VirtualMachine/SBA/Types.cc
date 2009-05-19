/** \file Types.cc

 \brief Gannet Service-based SoC project - SystemC NoC Transmitter/Receiver module

*/

/* ***** BEGIN LICENSE BLOCK *****
 * Version: AFL 2.1
 *
 * The contents of this file are subject to the Academic Free License Version
 * 2.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://opensource.org/licenses/afl-2.1.php
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 *
 *
 * ***** END LICENSE BLOCK ***** */

//==============================================================================
//
// NoC Transmitter/Receiver module
//
//==============================================================================

// $Id$
#ifndef STATIC_ALLOC
#include "Types.h"

using namespace SBA;

       	//* List Write operation
	void Store::mput(unsigned int address,Word_List data) {
		Store::storage[address]=data;
	}

	//* List Read operation
	Word_List Store::mget(unsigned int address) {
		return Store::storage[address];
	}

	//*  Write operation
	void Store::put(unsigned int address,Word data) {
		Store::storage[address].pop_back();
		Store::storage[address].push_back(data);
	}

	//*  Read operation
	Word Store::get(unsigned int address) {
		return Store::storage[address].at(0);
	}
 	//* Check if address is in use
	bool Store::has(unsigned int address) {
		return (Store::storage.count(address)==1);
	}
	//* Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void Store::remove(unsigned int address) {
#ifndef STATIC_ALLOC_
		map_iter iter=Store::storage.find(address);
		if (iter!=Store::storage.end()) {
			Store::storage.erase(iter);
		}
#else
		Store::storage[address]=0;
#endif
	}

	//* For monitoring
	unsigned int Store::utilized(void) {
#ifndef STATIC_ALLOC_
		return Store::storage.size();
#else
		return DATA_SZ;
#endif
	}

#endif // STATIC_ALLOC

/** \file Types.h
   
 \brief Gannet Service-based SoC project - General types for SBA
 
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
// General Types for Gannet
//
//==============================================================================

// $Id$

#ifndef _SBA_TYPES_H_
#define _SBA_TYPES_H_

#include <string>
#include <map>
#include <vector>
#include <deque>
#include <iostream>

//#define BOOST_ANY

#ifdef BOOST_ANY
#include <boost/any.hpp>
#endif

//#include "Base/ServiceCore.h"
using namespace std;

//For testing-specific code
#define WV_TEST
//* Convenience macro for iterating over STL datastrcutures. Iterator is always name iter, and runs always from begin() to end().
#define foreach(_datastructure_type_,_datastructure_name_) \
for (_datastructure_type_::iterator iter=_datastructure_name_.begin();iter!=_datastructure_name_.end();iter++)


typedef unsigned int uint; // most likely 32bit  

#ifdef DARWIN
typedef u_int8_t uint8;
typedef u_int16_t uint16;
typedef u_int32_t uint32;
//typedef u_int64_t uint64;
#else
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
//typedef u_int64_t uint64;
#endif

namespace SBA {
#ifdef DARWIN	
        typedef u_int64_t Uint64;
        typedef u_int32_t Uint32;
		typedef u_int16_t Uint16;        
		typedef u_int8_t Uint8;
#else		
        typedef uint64_t Uint64;
        typedef uint32_t Uint32;	 
		typedef uint16_t Uint16;        
		typedef uint8_t Uint8;
#endif
		typedef int64_t Sint64;        	
        typedef int32_t Sint32;
	#if WORDSZ==64        	
		typedef Uint64 Word;
		typedef Uint64 Label;
		typedef Sint64 Int;
		typedef double Float;  
	#elif WORDSZ==32 
		typedef Uint32 Word;
		typedef Uint32 Label;
		typedef Sint32 Int;
		typedef float Float;
	#endif			
			
		typedef Word Symbol_t;	

		typedef unsigned int CodeAddress; // typically fits into Task+Subtask			
		typedef unsigned int MemAddress; // typically fits into Subtask
		typedef MemAddress  ServiceAddress; 
		typedef MemAddress Subtask;
		typedef unsigned int Bit;
		typedef unsigned int Counter;
		typedef string ServiceString;
		typedef unsigned short int Service; // 2 bytes
		struct StringPair {
			string Taskfile;
#if DATA==1
			string Datafile;
#endif // DATA
		};
/** A template for a Perl/Ruby-style list with push/pop/shift/unshift/length methods
 * Note that pop() and shift() return the argument, unlike their STL counterparts.
 * shift/unshift operate at the front, push/pop at the back. So a proper FIFO uses only push/shift.
 * Of course all built-in deque methods are inherited as well.
 * Note that the deque<LType>:: qualifier is required for any inherited function
 *  with no arguments that depend on a template parameter
 */
template <typename LType> class List : public deque<LType> {
	public:	
//		unsigned int status;
		LType shift() {
			LType t_elt=deque<LType>::front();
			deque<LType>::pop_front();
//			if (deque<LType>::size()==0) status=0;
			return t_elt;
		}
//		void unshift(LType& elt) {
//			push_front(elt);						
//			status=1;
//		}		
		void push(LType& elt) {
			push_back(elt);
//			status=1;			
		}
/*
		void push_back(LType& elt) {
			push_back(elt);
			status=1;			
		}
*/
//		LType pop() {
//			LType t_elt=deque<LType>::back();
//			deque<LType>::pop_back();
//			if (deque<LType>::size()==0) status=0;			
//			return t_elt;			
//		}
//		unsigned int size() {
//			return deque<LType>::size();
//		}
		unsigned int length() {
			return deque<LType>::size();
		}
		void clear() {
			deque<LType>::clear();
//			status=0;
		}		
//		List() : status(0) {};
//		List(uint nelts) : deque<Word>(nelts), status(0) {};
}; // of List template		

template <typename LType> class Fifo : public deque<LType> {
	public:	
		unsigned int status;
		LType shift() {
			LType t_elt=deque<LType>::front();
			deque<LType>::pop_front();
			if (deque<LType>::size()==0) status=0;
			return t_elt;
		}
		void unshift(LType& elt) {
			push_front(elt);						
			status=1;
		}		
		void push(LType& elt) {
			push_back(elt);
			status=1;			
		}

		LType pop() {
			LType t_elt=deque<LType>::back();
			deque<LType>::pop_back();
			if (deque<LType>::size()==0) status=0;			
			return t_elt;			
		}

		unsigned int length() {
			return deque<LType>::size();
		}
		void clear() {
			deque<LType>::clear();
			status=0;
		}		
		Fifo() : status(0) {};
		Fifo(uint nelts) : deque<Word>(nelts), status(0) {};
}; // of Fifo template


//		typedef List<Word> Word_List;
		typedef deque<Word> Word_List;
		typedef Word_List Result;
		typedef Word_List Data;
		typedef Word_List Value; // WV: or Word?
		typedef Word_List Values;
		
		typedef List<double> Double_List;
		typedef List<MemAddress> MemAddresses;
		//typedef Word_List (*FuncPointer)(Base::ServiceCore*,MemAddresses&);			
		typedef List<StringPair> TaskDescList;
		typedef List<uint> Status_List;
		
		class ServicePair {
			public:
			//FuncPointer core;	
			ServiceAddress address;
			ServiceString name;
			ServicePair() {};
			//ServicePair(ServiceAddress a_,FuncPointer fp_,ServiceString n_) : core(fp_), address(a_), name(n_) {};
		}; 	
		
		class AliasPair {
			public:
			string name;
			ServiceAddress id;
			AliasPair() {};
			AliasPair(string a_, ServiceAddress i_) : name(a_), id(i_) {};
		}; 			
		
		typedef map<Word,Word> Address_Lookup; // Typically the address will be the Subtask field, so we can use the rest for status, offset etc
		typedef map<MemAddress,Word> Symbol_Table;		
		typedef map<CodeAddress,uint> CodeStatus_Table; 
		typedef map<Service,ServicePair> Services;
		typedef map<ServiceString,AliasPair> Aliases;	
		typedef map< Service,ServiceAddress > ServiceAddressLookup;

// SBA_Storage needs a header as we want to organise it as a linked list;
// even if we don't, we need to store some status information.
// So the first word of any Linked list mem block should contain some status information
// the first word of the chunk must contain a pointer to the next address,
// as well as a mask indicating 
// - this chunk is full/not full: 1 bit
// - this LL only contains a single value? or is every chunk a list element? 1 bit
// - status of the data in the chunk: 2 bits
// the "next address" should have a range of say 1M at least 
// so let's say we use a 20-bit word for the pointer and 4 bits for the mask
// Assuming chunk sizes of 64 words or less, the number of used words can be expressed in 6 bits (i.e. 2**6)
// Assuming 32-bit word storage, we also need 2 bits to indicate the length of the last word in bytes. if 0, it's 4
// So for a single-linked list we have 20+4+6+1+1=32. 
// Now create a nice convenient object for this header word
// 
// For the usual high-level model we actually only use Data_Status
// The SBA_Chunk_Header is always at address 0 for anything stored using mput.

class Chunk_Header {
	public:
	uint16 subtask:16;
	uint8 data_status:2;
	uint8 streaming:1; 
	uint8 list:1;
	uint padding:12; 	
#if USE_LL
	uint32 next_addr:20; 
	uint8 full:2;
	uint8 chunk_status:2; 	
	uint8 used_Words:6; 
	uint8 last_Word_size:2;
#endif

	Word to_word();	
#if USE_LL	
	Chunk_Header() : subtask(0), data_status(0),streaming(0), list(0), next_addr(0), full(0), chunk_status(0), used_Words(0), last_Word_size(0) {};
#else
	Chunk_Header() : subtask(0), data_status(0),streaming(0), list(0), padding(0) {};
#endif	
};		
		/** SBA::Storage is meant to be a generic model of a storage for arbitrary objects, hence the use of boost::any */
class Storage {
	private:
#ifndef BOOST_ANY
	map<unsigned int,Word_List> storage;
	map<unsigned int,Chunk_Header> header;	
	typedef map<unsigned int,Word_List>::iterator map_iter;
	typedef map<unsigned int,Chunk_Header>::iterator map_iter_h;	
#else
	map<unsigned int,boost::any> storage;
	typedef map<unsigned int,boost::any>::iterator map_iter;
#endif		
	public:
	/// Initialise header
	void init(unsigned int address);
	
	uint8 status(unsigned int address);
	void status(unsigned int address, uint8 data_status);
	
	uint16 subtask(unsigned int address);
	void subtask(unsigned int address, uint16 subtask);

	uint8 streaming(unsigned int address);
	void streaming(unsigned int address, uint8 streaming);

	uint8 list(unsigned int address);
	void list(unsigned int address, uint8 list);		
		
	/// List Write operation	
	void mput(unsigned int address,Word_List data);
	/// List Read operation
	Word_List mget(unsigned int address);
	
	// RAM-FIFO Push operation for list of words
	void mpush(unsigned int address,Word_List data);
    // RAM-FIFO Shift operation for list of words
    Word_List mshift(unsigned int address);
        // RAM-FIFO Head operation for list of words
    Word_List mhead(unsigned int address);
	
	
	/// Write operation	
	void put(unsigned int address,Word data);
	/// Read operation
	Word get(unsigned int address);
	/// Check if address is in use
	bool has(unsigned int address);
	/// Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address);
	/// For monitoring
	unsigned int utilized(void);
	

}; // end of Storage class definition

} // SBA
#endif /*_SBA_TYPES_H_*/

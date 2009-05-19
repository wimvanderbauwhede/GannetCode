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
#include <stdlib.h> // for exit()
#include <string>

#include <map> // for Lookup, FIXME!
#include <deque> // for TaskDescList, FIXME!


#ifndef STATIC_ALLOC

#endif

#if USE_THREADS==1
#include <pthread.h>
//#include <boost/thread/thread.hpp>
//#include <boost/thread/mutex.hpp>
#endif

#include <iostream> // for cerr & cout!!

#include "Base/ServiceCore.h"
#include "ServiceConfiguration.h"

using namespace std;

//* Convenience macro for iterating over STL datastrcutures. Iterator is always name iter, and runs always from begin() to end().
#define foreach(_datastructure_type_,_datastructure_name_) \
for (_datastructure_type_::iterator iter=_datastructure_name_.begin();iter!=_datastructure_name_.end();iter++)

namespace SBA {
		typedef Word Symbol_t;

		typedef unsigned int CodeAddress; // typically fits into Task+Subtask
		typedef unsigned int MemAddress; // typically fits into Subtask
		typedef MemAddress  ServiceAddress;
		typedef MemAddress Subtask;
		typedef unsigned int Bit;
		typedef unsigned int Counter;
		typedef unsigned short int Service; // 2 bytes

// FIXME: must be replaced by a custom lookup table, see Lookup_table.cc
//#ifndef STATIC_ALLOC
//		typedef map<Word,Word> LookupTable; // Typically the address will be the Subtask field, so we can use the rest for status, offset etc

//		typedef map<Word,Word> Address_Lookup; // Typically the address will be the Subtask field, so we can use the rest for status, offset etc
//#else
		//typedef LookupTable Address_Lookup;
//#endif
		struct StringPair { // used in Gateway
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
#ifndef STATIC_ALLOC
template <typename LType> class List : public deque<LType> {
	public:
		LType shift() {
			LType t_elt=deque<LType>::front();
			deque<LType>::pop_front();
			return t_elt;
		}
		void push(LType& elt) {
			push_back(elt);
		}
		unsigned int length() {
			return deque<LType>::size();
		}
		void clear() {
			deque<LType>::clear();
		}
		List() {};

		List(deque<LType>& d_) {
//			(*this)(d_);
			this->swap(d_);
		};
}; // of List template

// depth is ignored for dynamic alloc
template <typename LType, Word depth> class Fifo : public deque<LType> {
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
}; // of Fifo template





#else // STATIC_ALLOC
template <typename LType, Word depth> class Fifo {
private:
	LType mem[depth];
	Word push_pointer;
	Word shift_pointer;
public:
 	unsigned int status;
	Fifo () :  push_pointer(0), shift_pointer(0), status(0) {};

	void push(LType w) {
	    mem[push_pointer]=w;
	    if (push_pointer==depth-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
#ifdef VERBOSE
	    if (push_pointer>depth-1) {
	    	cerr << "Overflow: "<<push_pointer<<">"<<depth-1<<"\n";
	    	cout << "Overflow: "<<push_pointer<<">"<<depth-1<<"\n";
	    }
#endif //  VERBOSE
	    status=1;
	}

	LType shift() {
	    LType w= mem[shift_pointer];

	    if (shift_pointer==depth-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    unsigned int len=length();
	    if (len==0) status=0;
	    return w;
	}

	LType unguarded_shift() {
	    LType w= mem[shift_pointer];
	    shift_pointer++;
	    return w;
	}

	LType pop() {
	    LType w=mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        push_pointer--;
	    }
	    unsigned int len=length();
	    if (len==0) status=0;
	    return w;
	}

	void unshift(LType w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        shift_pointer=depth-1;
	    } else {
	        shift_pointer--;
	    }
	    status=1;
	}

	bool empty() {
	    return (push_pointer==shift_pointer);
	}
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int length() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+depth;
	    } else {
	    	return 0;
	    }
	}
	unsigned int size() {
		return length();
	}

	bool full() {
	    return length()==depth;
	}

	void clear() {
		push_pointer=0;
		shift_pointer=0;
		status=0;
	}
};
// This is a static implementation of a STL-style list. Main differences:
// fixed length
// no iterators
// assigning to at() or [] is not possible
template <typename LType, Word max_length> class List {
private:
	LType mem[max_length];
	uint push_pointer;
	uint shift_pointer;

public:
	List () : push_pointer(0), shift_pointer(0) {};
	List (uint length_) : push_pointer(0), shift_pointer(0) {}; // purely for byc2payload, to be compatible with dynamic case

	void push_back(LType w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==max_length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	void push(LType w) { // push
	    push_back(w);
	}

	LType back() {// combine with pop_back() to do pop()
	    return mem[push_pointer];
	}
	void pop_back() { // combine with back() to do pop()
	    if (push_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        push_pointer--;
	    }
	}

	LType pop() {
		LType w = back();
		pop_back();
		return w;
	}

	LType front() {// combine with pop_front() to do shift()
	    return mem[shift_pointer];
	}

	void pop_front() { // combine with front() to do shift()
	    if (shift_pointer==max_length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	}

	LType shift() {
		LType w = front();
		pop_front();
		return w;
		/*
	    LType w= mem[shift_pointer];
	    if (shift_pointer==max_length-1) {
	        push_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    return w;*/
	}



	LType& at(unsigned int address) {
		return mem[address];
	}
/*
	LType pop() {
	    LType w = mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        push_pointer--;
	    }
	    return w;
	}
*/
	void push_front(LType w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        push_pointer=max_length-1;
	    } else {
	        shift_pointer--;
	    }
	}

	void unshift(LType w) {
		push_front(w);
	}
/*
	bool empty() {
	    return (push_pointer==shift_pointer);
	}
*/
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int size() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+max_length;
	    } else {
	    	return 0;
	    }
	}

	void clear() {
		push_pointer=0;
		shift_pointer=0;
	}
          
 	inline const LType& operator[] (const unsigned int i) const {
		return mem[i];
 	}
};

template <unsigned int length> class Static_Word_List {
private:
	Word mem[length];
	Word push_pointer;
	Word shift_pointer;

public:
	Static_Word_List () : push_pointer(0), shift_pointer(0) {};
	Static_Word_List (Uint16 length_) : push_pointer(0), shift_pointer(0) {}; // purely for byc2payload, to be compatible with dynamic case

	void push_back(Word w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	void push(Word w) { // push
	    mem[push_pointer]=w;
	    if (push_pointer==length-1) {
	        push_pointer=0;
	    } else {
	        push_pointer++;
	    }
	}

	Word shift() {
	    Word w= mem[shift_pointer];
	    if (shift_pointer==length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	    return w;
	}

	Word back() {// combine with pop_back() to do pop()
	    return mem[push_pointer];
	}
	void pop_back() { // combine with back() to do pop()
	    if (push_pointer==0) {
	        push_pointer=length-1;
	    } else {
	        push_pointer--;
	    }
	}
	void pop_front() { // combine with front() to do shift()
	    if (shift_pointer==length-1) {
	        shift_pointer=0;
	    } else {
	        shift_pointer++;
	    }
	}
	Word front() {// combine with pop_front() to do shift()
	    return mem[shift_pointer];
	}
	Word at(unsigned int address) {
		return mem[address];
	}

	void at(unsigned int address,Word val) {
		mem[address]=val;
	}


/*
	Word pop() {
	    Word w = mem[push_pointer];
	    if (push_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        push_pointer--;
	    }
	    return w;
	}

	void unshift(Word w) {
	    mem[shift_pointer]=w;

	    if (shift_pointer==0) {
	        push_pointer=depth-1;
	    } else {
	        shift_pointer--;
	    }
	}

	bool empty() {
	    return (push_pointer==shift_pointer);
	}
*/
	// e.g. 8 spaces; 4,5,6,7,0 in use
	// occ is 5, but 0-4+1 = -3
	unsigned int size() {
		if (push_pointer>shift_pointer) {
	    	return push_pointer-shift_pointer;
	    } else if (push_pointer<shift_pointer) {
	    	return push_pointer-shift_pointer+length;
	    } else {
	    	return 0;
	    }
	}
/*
	bool full() {
	    return occ()==depth;
	}

	unsigned int  status() {
			return (empty())?0:1;
	}
*/
	void clear() {
		push_pointer=0;
		shift_pointer=0;
	}

 	inline const Word& operator[] (const unsigned int i) const {
		return mem[i];
 	}
/*
 	const Word* payload()  const {
 		return mem+3;
 	}
 	const Word* header()  const {
 		return mem;
 	}
*/

	Static_Word_List<length> payload() {
		Static_Word_List<length> payload=*this;
		payload.shift_pointer+=3;
		return payload;
	}
/*
typedef unsigned int iterator;

unsigned int begin() {
	return shift_pointer;
}

unsigned int end() {
	return push_pointer;
}
*/
};

// This is a simple Array class, a trick really to pass arrays around
// Works only for numbers!!!
template <typename LType, Word size_> class Array {
private:
	LType mem[size_];
	Word length;
public:
	Array () : length(0) {
		for (uint i=0;i<size_;i++) {
			mem[i]=0;
		}
	}
	// Note that the return value must be a ref to be able to assign to it
 	inline LType& operator[] (const unsigned int i) {
		return mem[i];
 	}
 	// This is tricky: size() is required for compatibility with STL
 	// But it must be set externally!
 	void size(Word l) {
 		length=l;
 	}
 	Word size() {
 		return length;
 	}

 	void clear() {
		length=0;
		for (uint i=0;i<size_;i++) {
			mem[i]=0;
		}
 	}
};

#endif // STATIC_ALLOC

#ifndef STATIC_ALLOC
		typedef List<Word> Word_List;
		typedef deque<Word> List_Store;
		typedef deque<Word> Word_Fifo;
		typedef List<Word> Bytecode;
		typedef List< Bytecode > BytecodeQueue;
		typedef deque<MemAddress> MemAddresses; // was List
		typedef map<MemAddress,Word> Symbol_Table;
		typedef map<CodeAddress,uint> CodeStatus_Table;
#else // STATIC_ALLOC
		typedef Static_Word_List<MAX_PACKET_SZ> Word_List;
		typedef Static_Word_List<MAX_LIST_SZ> List_Store;
		typedef Fifo<Word,MAX_PACKET_SZ> Word_Fifo;
		typedef List<Word,MAX_BYC_SZ> Bytecode;
		typedef List< Bytecode, MAX_NPENDING_TASKS > BytecodeQueue;
		typedef Array< MemAddress, MAX_NARGS > MemAddresses;
		typedef Array<Word,DATA_SZ> Symbol_Table;
		typedef Array<uint,CODE_SZ> CodeStatus_Table;

#endif 	 // STATIC_ALLOC

		typedef Word_List Result;
		typedef Word_List Data;
		typedef Word_List Value; // WV: or Word?
		typedef Word_List Values;

		typedef Word_List (*FuncPointer)(Base::ServiceCore*,MemAddresses&);
		//typedef Word_List (*SC_FuncPointer)(void*,Uint,Word_List&);
		typedef Word_List (*SC_FuncPointer)(void*,MemAddresses&);
		class ServicePair {
			public:
#ifndef SYSC
			FuncPointer core;
			ServiceAddress address;
			ServicePair() {};
			ServicePair(ServiceAddress a_,FuncPointer fp_) : core(fp_), address(a_) {};
#else
			SC_FuncPointer core;
			ServiceAddress address;
			uint t_setup;
			uint t_proc_value;
			ServicePair() {};
			ServicePair(ServiceAddress a_,SC_FuncPointer fp_) : core(fp_), address(a_), t_setup(1), t_proc_value(0)  {};
			ServicePair(ServiceAddress a_,SC_FuncPointer fp_,uint ts_, uint tp_ ) : core(fp_), address(a_), t_setup(ts_), t_proc_value(tp_) {};
#endif
		};

        typedef deque<StringPair> TaskDescList; // WV27-82008: TODO: get rid of this

#ifndef STATIC_ALLOC
	typedef Word_List Packet_t;
	typedef Word_List Header_t;
	typedef List<Packet_t> Packet_List;
#else
	typedef Static_Word_List<MAX_PACKET_SZ> Packet_t;
	typedef Static_Word_List<HEADER_SZ> Header_t;
	typedef List<Packet_t,PACKET_FIFO_SZ> Packet_List;
#endif // STATIC_ALLOC
	typedef Word_List Payload_t;

	typedef Fifo<Packet_t,PACKET_FIFO_SZ> Packet_Fifo;

	typedef Packet_Fifo TX_Packet_Fifo;

#if USE_THREADS==0
	typedef Packet_Fifo TRX_Packet_Fifo;
	typedef Packet_Fifo RX_Packet_Fifo;
#else // USE_THREADS==1

/*
// length|clear|
// Code borrowed from by Anthony Williams (11 December 2008)
// http://www.justsoftwaresolutions.co.uk/threading/implementing-a-thread-safe-queue-using-condition-variables.html
class RX_Packet_Fifo
{
private:
    std::deque<Packet_t> packets;
    mutable boost::mutex the_mutex;
    boost::condition_variable the_condition_variable;
public:
 	unsigned int status;
	TRX_Packet_Fifo () :  status(0) {};

    void push(Packet_t const& data)
    {
        boost::mutex::scoped_lock lock(the_mutex);
        packets.push_back(data);
        status=1;
        lock.unlock();
        the_condition_variable.notify_one();
    }

    bool empty() const
    {
        boost::mutex::scoped_lock lock(the_mutex);
        return packets.empty();
    }

    unsigned int size() const
    {
        boost::mutex::scoped_lock lock(the_mutex);
        return packets.size();
    }

    Packet_t shift()
    {
        boost::mutex::scoped_lock lock(the_mutex);
        while(packets.empty())
        {
            the_condition_variable.wait(lock);
        }

        Packet_t t_elt=packets.front();
        packets.pop_front();
		if (empty()) status=0;
		return t_elt;
    }
	void clear() {
		packets.clear();
	}
};
*/
// without boost

class RX_Packet_Fifo
{
private:
    std::deque<Packet_t> packets;

    pthread_mutex_t the_mutex;    pthread_cond_t  the_condition_variable;

public:
 	unsigned int status;
	RX_Packet_Fifo () :  status(0) {
        pthread_mutex_init(&the_mutex, NULL);        pthread_cond_init(&the_condition_variable, NULL); // was 0
	};
    ~RX_Packet_Fifo() {        pthread_cond_destroy(&the_condition_variable);        pthread_mutex_destroy(&the_mutex);    }

    bool has_packets() {
      // we want to block until the status is true
      // so status() will block until it can return true
        while(packets.empty())
        {
            pthread_cond_wait(&the_condition_variable, &the_mutex);
        }
      status=1;
      return(1); // @status
	}

    void push(Packet_t const& data)
    {
        pthread_mutex_lock(&the_mutex);
        packets.push_back(data);
        status=1;
        pthread_mutex_unlock(&the_mutex);
        pthread_cond_signal(&the_condition_variable); // only 1 thread should be waiting
    }
/*
    bool empty() {
        pthread_mutex_lock(&the_mutex);
//        boost::mutex::scoped_lock lock(the_mutex);
        return packets.empty();
        pthread_mutex_unlock(&the_mutex);
    }
*/
    unsigned int length() { // do we need to lock before checking the length?
//        pthread_mutex_lock(&the_mutex);
        return packets.size();
//        pthread_mutex_unlock(&the_mutex);
    }

    unsigned int size() { // do we need to lock before checking the length?
//        pthread_mutex_lock(&the_mutex);
        return packets.size();
//        pthread_mutex_unlock(&the_mutex);
    }
/*
   We don't really want to block on shift()
 So I have to check when the fifo has 1 elt, then
 set status to 0 and shift that elt. As we check status we won't come back there
 then we use the blocking call simply to set the status back to 1
*/
    Packet_t shift() {
        pthread_mutex_lock(&the_mutex);

        if (packets.size()==1) {
        	status=0;
        }
        Packet_t t_elt=packets.front();
        packets.pop_front();
        pthread_mutex_unlock(&the_mutex);

		return t_elt;
    }

	void clear() {
		packets.clear();
	}
}; // RX_Packet_Fifo

#endif // USE_THREADS

// ------------------------------------------------

#ifndef STATIC_ALLOC
		typedef map<Service,ServicePair> Services;
#else // STATIC_ALLOC
		class Services {
		private:
			ServicePair mem[MAX_NSERVICES];
		public:
			Services () {}
			// Note that the return value must be a ref to be able to assign to it
 			inline ServicePair& operator[] (const unsigned int i) {
				return mem[i];
 			}
		};

#endif 	 // STATIC_ALLOC

		/** SBA::Store is meant to be a generic model of a storage for arbitrary objects, hence the use of boost::any */
#ifndef STATIC_ALLOC
class Store {
#else
template <uint size> class Store {
#endif

	private:

#ifndef STATIC_ALLOC
	map<unsigned int,Word_List> storage;
	typedef map<unsigned int,Word_List>::iterator map_iter;
#else
	Word_List storage[size];
#endif // STATIC_ALLOC

	public:
#ifdef STATIC_ALLOC

       	//* List Write operation
	void mput(unsigned int address,Word_List data) {
		storage[address]=data;
	}

	//* List Read operation
	Word_List mget(unsigned int address) {
		return storage[address];
	}

	//*  Write operation
	void put(unsigned int address,Word data) {
		storage[address].pop_back();
		storage[address].push_back(data);

	}

	//*  Read operation
	Word get(unsigned int address) {
		return storage[address].at(0);
	}
	//* Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address) {
		storage[address]=0;
	}

	//* For monitoring
	unsigned int utilized(void) {
		return size;
	}


#else // not STATIC_ALLOC

	/// List Write operation
	void mput(unsigned int address,Word_List data);
	/// List Read operation
	Word_List mget(unsigned int address);
#ifdef RAM_FIFO
	// RAM-FIFO Push operation for list of words
	void mpush(unsigned int address,Word_List data);
    // RAM-FIFO Shift operation for list of words
    Word_List mshift(unsigned int address);
        // RAM-FIFO Head operation for list of words
    Word_List mhead(unsigned int address);
#endif // RAM_FIFO

	/// Write operation
	void put(unsigned int address,Word data);
	/// Read operation
	Word get(unsigned int address);
#ifndef STATIC_ALLOC_
	/// Check if address is in use
	bool has(unsigned int address);
#endif
	/// Remove address. This could mean setting the "in_use" bit to 0 or setting the whole word to 0
	void remove(unsigned int address);
	/// For monitoring
	unsigned int utilized(void);
#endif // STATIC_ALLOC

}; // end of Store class definition

} // SBA
#endif /*_SBA_TYPES_H_*/

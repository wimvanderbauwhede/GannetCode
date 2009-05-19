/*
 * SC_Word_List.h
 *
 *  Created on: 15-Dec-2008
 *      Author: StudentAdmin
 */

#ifndef SC_WORD_LIST_H_
#define SC_WORD_LIST_H_

#include "SC_sba.h"


namespace SC_SBA{


// Since the SBA::Word_List is of type SBA::List<Word>, and
// it is like SBA::Fifo, based on stl::deque, but only with
// lesser access methods defined,
// so to get going at least, we can simply define SC_Word_List
// to be the same as SC_Fifo<Word>
//typedef SC_Fifo<SBA::Word, PACKET_FIFO_SZ>    SC_Word_List;
typedef SC_Deque<SBA::Word>    SC_Word_List;
//typedef unsigned int uint; // most likely 32bit

}//namespace:SC_SBA

#endif /* SC_WORD_LIST_H_ */

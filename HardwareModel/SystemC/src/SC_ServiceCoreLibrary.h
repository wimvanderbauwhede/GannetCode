// ServiceCoreLibrary.rb
//   
// :title: Service-based SoC project - Service Core Library 
//
//    This version of Gannet is for compile-time decomposition and lexical scoping.
//    It does not work with the "old" system any more!!
//    This is the version to be ported to SystemC
//    -"values" is deprecated, we pass on addresses!
//    -The language services are minimal:
//    LET x
//    ASSIGN x
//    LAMBDA 
//    APPLY 
//    IF x
//    We'll keep BEGIN, though I see no use for it
//    -List manipulation services need a total rework. Initially, we'll support
//    LIST
//    HEAD
//    TAIL
//    LENGTH
//    CONCAT
//    and maybe CONS
//    
//    -ALU services are as before: x
//    +,-,*,/;<,==,>
//    Guess we should add AND, OR, NOT
//    
//
///* ***** BEGIN LICENSE BLOCK *****
// * Version: AFL 2.1
// *
// * The contents of this file are subject to the Academic Free License Version
// * 2.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://opensource.org/licenses/afl-2.1.php
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
// *
// * ***** END LICENSE BLOCK ***** */
//
//
//// $Id: ServiceCoreLibrary.rb 2532 2009-04-22 16:15:08Z socgroup $

// ****** Code generated from SBA/ServiceCoreLibrary.rb by ./r2n.pl ******
// ****** DO NOT EDIT (unless you know what you're doing) ******


#ifndef SC_SERVICECORELIBRARY_H_
#define SC_SERVICECORELIBRARY_H_


#ifndef SYSC
#include "Types.h" 
#include "Packet.h" 
#include "Base/ServiceCore.h" 
#include "cs_DCT.h" 
#endif


#include "SC_sba.h"
			
using namespace std;

namespace SC_SBA {
#include "SC_ServiceCore_signature.h"
namespace SCLib {




#ifndef NO_SERVICES
 Word_List string2symbol(string); 

 string sym2str(Word_List); 

 Int sym2int(Word_List); 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ                
    
 Word sym2uint(Word_List); 
    
 bool sym2bool(Word_List); 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ                
                    
 float sym2flt(Word_List); 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ
#endif // NO_SERVICES        


 Result sba_GATEWAY(void*,MemAddresses&); 



#ifndef NO_SERVICES    
 Int div(Int,Int); 
    
    
 Word_List ls_ALU(void*,MemAddresses&); 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ                
    
#if 0 // SKIP ls_COUNTER
 Word_List ls_COUNTER(void*,MemAddresses&); 
    
#endif // 0
#if 0 // SKIP ls_COUNTDOWN
 Word_List ls_COUNTDOWN(void*,MemAddresses&); 

#endif // 0
#if 0 // SKIP ls_FIB
 Word_List ls_FIB(void*,MemAddresses&); 
            
#endif // 0
#if 0 // SKIP ls_THREAD
 Word_List ls_THREAD(void*,MemAddresses&); 

       
        
#endif // 0
#if 0 // SKIP ls_DELAY
 Word_List ls_DELAY(void*,MemAddresses&); 

#endif // 0
#if 0 // SKIP ls_HWTHREAD
 Word_List ls_HWTHREAD(void*,MemAddresses&); 

    
    
#endif // 0
 Word_List ls_BEGIN(void*,MemAddresses&); 
    
    
#if 0 // SKIP ls_DATA
 Word_List ls_DATA(void*,MemAddresses&); 
#endif // 0
 Word_List ls_IF(void*, MemAddresses&); 
#if 0 // SKIP ls_S_IF
 Word_List ls_S_IF(void*, MemAddresses&); 

#endif // 0
 Word_List ls_RAND(void*, MemAddresses&); 


#if 0 // SKIP ls_RND_MATRIX
 Word_List ls_RND_MATRIX(void*, MemAddresses&); 


#endif // 0
#if 0 // SKIP ls_PROC_MATRIX
 Word_List ls_PROC_MATRIX(void*, MemAddresses&); 
 
#endif // 0
 Word_List ls_LET(void*, MemAddresses&); 
#if 0 // SKIP ls_LAMBDA
 Word_List ls_LAMBDA(void*, MemAddresses&); 



#endif // 0
#if 0 // SKIP ls_APPLY
 Word_List ls_APPLY(void*, MemAddresses&); 
#endif // 0
 Word_List ls_IO(void*,MemAddresses&); 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ 
#if WORDSZ==64
#else // WORDSZ==32
#endif // WORDSZ
#if 0 // SKIP cs_DCT
 Word_List cs_DCT(void*,MemAddresses&); 


#endif // 0
Result none(void*,MemAddresses&); 
        
#endif // NO_SERVICES
}; // SCLib
} // namespace SC_SBA
#include "SC_ServiceCoreLibrary.cc"
#endif // SC_SERVICECORELIBRARY_H_

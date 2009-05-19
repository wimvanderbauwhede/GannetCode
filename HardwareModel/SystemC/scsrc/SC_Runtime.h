/*
********************************************************************************
                 |
  File Name      | SC_Runtime.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 29-Oct-2008. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC Gannet Runtime Module
-----------------|--------------------------------------------------------------
  Modifications  | WN_20081029: Created.
                 |
********************************************************************************
*/
// * ***** BEGIN LICENSE BLOCK *****
// * Version: AFL 2.1
// *
// * The contents of this file are subject to the Academic Free License Version
// * 2.1 (the "License") you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://opensource.org/licenses/afl-2.1.php
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// *  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
// *
// * ***** END LICENSE BLOCK ***** */

#ifndef SC_RUNTIME_H_
#define SC_RUNTIME_H_


//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "SC_sba.h"

//------------------------------------------------------------------------------
// NAMESPACES
//------------------------------------------------------------------------------
using namespace std;

//------------------------------------------------------------------------------
// DEFS
//------------------------------------------------------------------------------

//==============================================================================
//	CLASS: SC RUNTIME
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA{

//! The SystemC Gannet Runtime Module
    /*!
       Detailed description here...
    */
class SC_Runtime : public sc_module
{
public:
    // ---------------------------- PORTS --------------------------------------


	// ---------------------------- Sub-Modules --------------------------------
	SBA::TaskDescList      task_descriptions;  //!< List from C++ model
	SC_System              sba;               //!< ...
	SBA::Bytecode          bytecode;           //!< deque from C++ model

	// ---------------------------- METHODS ------------------------------------
//	void do_proc(); //TODO: redundant?

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_Runtime"; }

	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_Runtime);
	SC_Runtime(    sc_module_name nm   ,
                   TaskDescList& td_)   :
        sc_module(nm)  ,
        task_descriptions(td_),
	    //sba("sba", task_descriptions)
        sba("sba", td_)
	{
        const_debug_msg(name(),kind());
		//SC_THREAD(do_proc);
	}
	// ---------------------------- Primitive Members --------------------------


};/* class: SC_Runtime */

/*
//==============================================================================
//	DO_PROC()
//==============================================================================

void SC_Runtime :: do_proc()
{
    //debug message
    //run_debug_msg(name());

}// funct: SC_Runtime :: do_proc()
*/

} /* namespace: SC_SBA */


#endif /* SC_RUNTIME_H_ */

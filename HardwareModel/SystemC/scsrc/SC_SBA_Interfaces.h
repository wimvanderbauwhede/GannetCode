//  The sba interface defintions
/*
************************************************************************************************************
*                 |
*  File Name      | SC_SBA_Interfaces.h
*-----------------|-----------------------------------------------------------------------------------------
*  Project        | SBA Library
*-----------------|-----------------------------------------------------------------------------------------
*  Created        | December 2008. Livingston, Scotland
*-----------------|-----------------------------------------------------------------------------------------
*  Origial Author | Syed Waqar Nabi, System Level Integration Ltd.
*                 |
*-----------------|-----------------------------------------------------------------------------------------
*  Description    | Interface declarations for the SBA
*-----------------|-----------------------------------------------------------------------------------------
*  Remarks        |
*                 |
*-----------------|-----------------------------------------------------------------------------------------
*  Modifications  |
*                 |
*                 |
*                 |
************************************************************************************************************
*/

#ifndef SBA_INTERFACE_H
#define SBA_INTERFACE_H

#include "SC_sba.h"

namespace SC_SBA
{
//TODO: Converts these dereturn_tofines to ingerited interface classes
//--------------------------------------- INTERFACES FOR COMM INSIDE THE SERVICE TILE ----------------------

//==============================================================================
//  CLASS: SC_fifo_put_if
//==============================================================================

//! The SC_fifo_put_if
/*!
    We are using uni-directional interfaces which represents the
    way communivation between modules and the FIFOs goes on inside
    the service manager. There is not handshaking/ack signals on FIFO reads
    or write insude the service manager.
*/
#define SC_fifo_put_if tlm::tlm_fifo_put_if

//==============================================================================
//  CLASS: SC_fifo_get_if
//==============================================================================

//! The SC_fifo_get_if
/*!
    Details here...
*/
#define SC_fifo_get_if tlm::tlm_fifo_get_if

//==============================================================================
//  CLASS: SC_fifo_delayed_put_if - DEPRECATED
//==============================================================================

//! The SC_fifo_delayed_put_if - DEPRECATED
/*!
    defines a delayed_put interface - DEPRECATED
*/
template <typename DATA_T>
class SC_fifo_delayed_put_if : public virtual sc_interface
{
public:
    virtual void delayed_put(const DATA_T &t ) = 0;
public:
    virtual void push (const DATA_T &t ) {delayed_put(t);} // alias for compatibility with C++
};//class SC_fifo_delayed_put_if


//==============================================================================
//  CLASS: SC_fifo_delayed_get_if- DEPRECATED
//==============================================================================
//! The SC_fifo_delayed_get_if- DEPRECATED
/*!
    defined a delayed get interface- DEPRECATED
*/
template <typename DATA_T>
class SC_fifo_delayed_get_if : public virtual sc_interface
{
public:
    virtual DATA_T delayed_get( tlm::tlm_tag<DATA_T> *t = 0 ) = 0;
    virtual void delayed_get( DATA_T &t ){ t = delayed_get(); }
public:
    virtual void shift( DATA_T &t ){ t = delayed_get(); } // alias for compatibility with C++

};//class SC_fifo_delayed_put_if



//==============================================================================
//  CLASS: SC_Fifo_if
//==============================================================================

//! The SC_Fifo_if
/*!
    Details here...
*/
template <typename DATA_T>
class SC_Fifo_if  :
    public virtual sc_interface
{
public:
    virtual DATA_T          shift()             = 0;
    //virtual void            unshift(DATA_T&)    = 0;
    virtual void            push(DATA_T&)       = 0;
    //virtual DATA_T          pop()               = 0;
    virtual unsigned int    length()            = 0;
    virtual unsigned int    size()              = 0;
    virtual void            clear()             = 0;
};//class SC_Fifo_if





//==============================================================================
//  CLASS: SC_reg_in_if
//==============================================================================
//! The SC_reg_in_if
/*!
    defined a read only interface to objects of type SC_Registers (based in sc_signals) - untimed
*/

#define SC_reg_in_if sc_signal_in_if

//==============================================================================
//  CLASS: SC_reg_in_if_d
//==============================================================================
//! The SC_reg_in_if_d
/*!
    defined a read only interface to objects of type SC_Registers (based in sc_signals)
*/

template <typename DATA_T>
class SC_reg_in_if_d :
    public virtual sc_interface
{
public:
    // read the current value
    virtual  DATA_T& read() = 0;

    // get the value changed event
    virtual const sc_event& value_changed_event() const = 0;

};//class SC_reg_in_if


//==============================================================================
//  CLASS: SC_reg_out_if
//==============================================================================
//! The SC_reg_out_if
/*!
    defined a read only interface to objects of type SC_Registers (based in sc_signals) - untimed
*/

#define SC_reg_out_if sc_signal_out_if


//==============================================================================
//  CLASS: SC_reg_out_if_d
//==============================================================================
//! The SC_reg_out_if_d
/*!
    defined a write(and read) interface to objects of type SC_Registers (based in sc_signals)
*/

template <typename DATA_T>
class SC_reg_out_if_d :
    public SC_reg_in_if_d<DATA_T>
{
public:
    // write the new value
    virtual void write( const DATA_T& ) = 0;

    // overloaded assignment '=' operator
    virtual void operator = (const DATA_T&) = 0;

    // overloaded equality '==' operator for DATA_T
    virtual bool operator == (const DATA_T&) = 0;

    // SC_Register has implicit cast implemented so that
    // when it is used in a function argument, it returns its data value
    //virtual operator DATA_T&() = 0;


    // overloaded equality '==' operator for SC_Register<DATA_T>
    //virtual bool operator == (const SC_Register<DATA_T>&)  const = 0;

};//class SC_reg_out_if_d


//==============================================================================
//  CLASS: SC_Reg_If
//==============================================================================

//Just a simpler name for SC_reg_out_if_d
//template <typename DATA_T>
//typedef SC_reg_out_if_d<DATA_T> SC_Reg_If<DATA_T>;


//==============================================================================
//  CLASS: SC_SBA_master_if
//==============================================================================



//! The SC_SBA_master_if: for masters (putting req, getting rsp) inside the service tile
/*!
    This interface is for the master access inside the service manager (putting request, getting response).
    Is built on top of the tlm_master_if which in turn is an aggregation of:
           - tlm_put_if         (includes both blocking and non-blocking ifs)
           - tlm_get_peek_if    (includes both blocking and non-blocking ifs)
*/


#define SC_SBA_master_if tlm::tlm_master_if

//==============================================================================
//  CLASS: SC_SBA_slave_if
//==============================================================================

//! The SC_SBA_slave_if: for slaves (getting request, putting response) inside the service tile
/*!
    This interface is for the 'slaves' inside the service manager. They will get REQ and put RSP.
    Is built on top of the tlm_slave_if which in turn is an aggregation of:
           - tlm_put_if         (includes both blocking and non-blocking ifs)
           - tlm_get_peek_if    (includes both blocking and non-blocking ifs)
*/
#define SC_SBA_slave_if tlm::tlm_slave_if

//==============================================================================
//  CLASS: SC_SBA_transport_if
//==============================================================================

//! The SC_SBA_transport_if
/*!
    Details here...
*/
#define SC_SBA_transport_if tlm::tlm_transport_if

//==============================================================================
//  CLASS: SC_Memory_if
//==============================================================================

//! The SC_Memory_if
/*!
    defined a write(and read) interface to objects of type SC_Registers (based in sc_signals)
*/

template <typename ADDR_T, typename DATA_T>
class SC_Memory_if  :
    public virtual sc_interface
{
public:
    // RAM Write operation
    virtual void write(ADDR_T ,DATA_T) = 0;

    // RAM Read operation
    // Return by reference, so read operation can be used to write as well
    // (as used in the overloaded [] operator
    virtual DATA_T& read(ADDR_T) = 0;


    virtual unsigned int size(ADDR_T) = 0;
    // Check if address is free
    virtual bool is_free(ADDR_T) = 0;

    // overloading the subscript '[]' operator
    virtual DATA_T&         operator [] (ADDR_T )       = 0;//non-const
    //virtual const DATA_T&   operator [] (ADDR_T ) const = 0;//const


};//class SC_Memory_if

//==============================================================================
//  CLASS: SC_SubtaskList_if
//==============================================================================

//! The SC_SubtaskList_if
/*!
    A detailed interface for accessing a SC_Subtask_List. Defines 'finer-grained' access
    methods that emulate byte-level access into the data in a list entry.
    The interface methods defined correspond to the access methods as defined in the
    SBA::Subtask_List object.
    The intention is that accessign an SC_Subtask_List should as much as possible be the same
    as accessing an SBA::Subtask_List object - for ease of porting C++ to SystemC.
*/

class SC_SubtaskList_if  :
    public virtual sc_interface
{
public:

//    SubtaskMap subtasks_list;
    virtual void                    lock                 ()=0;
    virtual void                    unlock              ()=0;

    virtual void                    add                 (const Subtask subtask)=0;
    virtual void                    remove              (const Subtask subtask)=0;
    virtual Subtask_Argument_List&  arguments           (const Subtask subtask)=0;
    virtual void                    add_arg           (const Subtask subtask,MemAddress address)=0;
    virtual Subtask_Status          status              (const Subtask subtask)=0;
    virtual void                    status              (const Subtask subtask, Subtask_Status status_)=0;
    virtual Word                    return_as           (const Subtask subtask)=0;
    virtual void                    return_as           (const Subtask subtask,Word return_as_)=0;
    virtual Symbol_t                called_as           (const Subtask subtask)=0;
    virtual void                    called_as           (const Subtask subtask,Symbol_t called_as)=0;
    virtual Service                 to                  (const Subtask subtask)=0;
    virtual void                    to                  (const Subtask subtask, Service to)=0;
    virtual Service                 return_to           (const Subtask subtask)=0;
    virtual void                    return_to           (const Subtask subtask, Service return_to)=0;
    virtual Word                    ack_to              (const Subtask subtask)=0;
    virtual void                    ack_to              (const Subtask subtask, Word ack_to)=0;
    virtual uint                    redir               (const Subtask subtask)=0;
    virtual void                    redir               (const Subtask subtask, uint redir)=0;
    virtual uint                    waiting_for_ack     (const Subtask subtask)=0;
    virtual void                    waiting_for_ack     (const Subtask subtask, uint waiting_for_ack)=0;
    virtual CodeAddress             code_address        (const Subtask subtask)=0;
    virtual void                    code_address        (const Subtask subtask, CodeAddress code_address)=0;
    virtual uint                    service_id          (const Subtask subtask)=0;
    virtual void                    service_id          (const Subtask subtask, uint service_id)=0;
    virtual uint                    nargs               (const Subtask subtask)=0;
    virtual void                    nargs               (const Subtask subtask, uint val)=0;
    virtual uint                    nargs_absent        (const Subtask subtask)=0;
    virtual void                    nargs_absent        (const Subtask subtask, uint val)=0;
    virtual void                    decr_nargs_absent   (const Subtask subtask)=0;
    virtual void                    incr_nargs_absent   (const Subtask subtask)=0;
    virtual uint                    mode                (const Subtask subtask)=0;
    virtual void                    mode                (const Subtask subtask, uint val)=0;
    virtual uint                    reg                 (const Subtask subtask)=0;
    virtual void                    reg                 (const Subtask subtask, uint val)=0;
    virtual uint                    offset                 (const Subtask subtask)=0;
    virtual void                    offset                 (const Subtask subtask, uint val)=0;
    virtual uint                    fsize                 (const Subtask subtask)=0;
    virtual void                    fsize                 (const Subtask subtask, uint val)=0;

    virtual Subtasks                subtasks            ()=0;
    virtual Subtask_List_Item       entry               (const Subtask subtask)=0;

}; //class: SC_SubtaskList_if


//==============================================================================
//  CLASS: SC_Stack_if
//==============================================================================

//! The SC_Stack_if
/*!
    Details here...
*/

class SC_Stack_if  :
    public virtual sc_interface
{
public:
    //* Push a freed address back onto the stack.
    virtual void push(unsigned int) = 0;
    virtual void push_back(unsigned int) = 0;
    //* Pop an address to use off the stack.
    virtual unsigned int pop() = 0;
    //* Check if stack is empty
    virtual bool empty() = 0;
    virtual unsigned int size() = 0;
};//class SC_Stack_if


    //==============================================================================
    //  CLASS: SC_RequestTable_if
    //==============================================================================

    //! The SC_RequestTable_if
    /*!
        RequestTable is the table of pending requests to variables
    */

    class SC_RequestTable_if  :
        public virtual sc_interface
    {
    public:
        //* Push a request onto the list at the given address.
        virtual void push(const SBA::MemAddress,SBA::Word) = 0;
        //* Shift a request off the list at the given address.
        virtual SBA::Word shift(const SBA::MemAddress) = 0;
        virtual unsigned int size(const SBA::MemAddress) = 0;
    };//class SC_RequestTable_if


//==============================================================================
//  CLASS: SC_LookupTable_if
//==============================================================================

//! The SC_LookupTable_if
/*!
    Details here...
*/

class SC_LookupTable_if  :
    public virtual sc_interface
{
public:
    //* Write to an address
    virtual void write(const SBA::Uint64,SBA::Sint64) = 0;
    virtual void erase(const SBA::Uint64) = 0;
    //* Read an address .
    virtual SBA::Sint64 read(const SBA::Uint64) = 0;

    virtual unsigned int count(const SBA::Uint64) = 0;
    virtual unsigned int size() = 0;
};//class SC_LookupTable_if

//==============================================================================
//  CLASS: SC_Config_if
//==============================================================================

//! The SC_Config_if to allow port-access to a SBA::Config object
/*!
    Details here...
*/

class SC_Config_if  :
    public virtual sc_interface
{
public:
     // The Config object contains a map of <Service, ServicePair>
    // which is called 'services'
    // this read function accepts  key to this map (of type ~Service) and returns a REFERENCE
    // to the value )of type ServicePair
    // the reading entity can then do what it wants with it.
    virtual SBA::ServicePair& read (SBA::Service) = 0;

    // overloading the subscript '[]' operator
    virtual SBA::ServicePair& operator [] (SBA::Service)= 0;

};//class SC_Config_if


//==============================================================================
//  CLASS: SC_Deque_if
//==============================================================================

//! The SC_Deque_if
/*!
    Details here...
*/
template <typename DATA_T>
class SC_Deque_if:
    public virtual sc_interface
{
public:
    virtual DATA_T          shift       () =0;
    virtual void            unshift     (DATA_T& )=0;
    virtual void            push        (DATA_T& )=0;
    virtual DATA_T          pop         ()=0;
    virtual DATA_T&         operator[]  (unsigned int ) =0;
    virtual DATA_T&         at          (unsigned int ) =0;
    virtual void            operator =  (const deque<DATA_T>&) =0;
    virtual unsigned int    length      () =0;
    virtual unsigned int    size        () =0;
    virtual void            clear       ()=0;
    virtual deque<DATA_T>&  read_all    ()=0;
};//class SC_Deque_if


}//namespace: SC_SBA

#endif /* SC_SBA_INTEFACES_H_ */

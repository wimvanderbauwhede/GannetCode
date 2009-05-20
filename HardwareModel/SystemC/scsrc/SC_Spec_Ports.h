//  The specialized ports defined in the SC_SBA model so that dot '.' operator can be used to access interface methods
/*
************************************************************************************************************
*                 |
*  File Name      | SC_Spec_Ports.h
*-----------------|-----------------------------------------------------------------------------------------
*  Project        | SBA Library
*-----------------|-----------------------------------------------------------------------------------------
*  Created        | 21-Dec-2008. Livingston, Scotland
*-----------------|-----------------------------------------------------------------------------------------
*  Origial Author | Syed Waqar Nabi, System Level Integration Ltd.
*                 |
*-----------------|-----------------------------------------------------------------------------------------
*  Description    | Specialized Port classes for use in the SystemC SBA model.
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

#ifndef SC_SPEC_PORTS_H_
#define SC_SPEC_PORTS_H_

#include "SC_sba.h"

namespace SC_SBA
{

//==============================================================================
//  CLASS: port_SC_Memory_if
//==============================================================================

//! Specialized port for accessing SC_Memory objects
/*!

*/
template <typename ADDR_T, typename DATA_T>
class port_SC_Memory_if :
    public sc_port<SC_Memory_if<ADDR_T, DATA_T>  >
{
public:
    // RAM Write operation
    void write(ADDR_T addr ,DATA_T data) { (*this) -> write(addr, data); }

    // write the new value
    void mput(ADDR_T addr ,DATA_T data) { (*this) -> write(addr, data); }

    // RAM Read operation
    // Return by reference, so read operation can be used to write as well
    // (as used in the overloaded [] operator
    DATA_T& read(ADDR_T addr) {return (*this) -> read(addr); }

    // read the current value
    DATA_T& mget(ADDR_T addr) {return (*this) -> read(addr); }

    // read the current value
    unsigned int size(ADDR_T addr) {return (*this) -> size(addr); }
    // Check if address is free
    bool is_free(ADDR_T addr) {return (*this) -> is_free(addr); }

    void remove(ADDR_T addr) { // do nothing

    }

    // overloading the subscript '[]' operator
    DATA_T&         operator [] (ADDR_T index)         { return (*this)->read(index); } //non-const
    //const DATA_T&   operator [] (ADDR_T index) const   { return (*this)->read(index); } //const


};//class SC_fifo_delayed_put_if


//==============================================================================
//  CLASS: port_SC_Fifo_if
//==============================================================================

//! Specialized port for accessing SC_Fifo_if methods
/*!
    Details here...
*/
template <typename DATA_T, int N = 1>
class port_SC_Fifo_if  :
    public sc_port<SC_Fifo_if<DATA_T>, N>
{
public:
    DATA_T          shift()                 { return (*this) -> shift() ; }
    //void            unshift(DATA_T& item)   { (*this) -> unshift(item); }
    void            push(DATA_T& item )     { (*this) -> push(item); }
    //DATA_T          pop()                   { return (*this) -> pop()   ; }
    unsigned int    length()                { return (*this) -> length(); }
    unsigned int    size()                  { return (*this) -> size(); }
    void            clear()                 { (*this) -> clear() ; }
};//class port_SC_Fifo_if

//==============================================================================
//  CLASS: port_SC_reg_if
//==============================================================================
//! Specialized port for accessing The SC_reg_out_if_d methods
/*!
    ...
*/

template <typename DATA_T>
class port_SC_reg_if :
    public sc_port <SC_reg_out_if_d<DATA_T> >
{
public:
    // write the new value
    void write( const DATA_T& val_) { (*this) -> write(val_); }


    // read the current value
    const DATA_T& read()            { return (*this) -> read(); }


    // get the value changed event
    const sc_event& value_changed_event() const
        { return (*this) -> value_changed_event(); }

    //overloaded assignment '=' operator
    void operator = (const DATA_T& rhs)
        { (*this)->write(rhs); }

    // overloaded equality '==' operator for DATA_T
    bool operator == (const DATA_T& rhs_)//  const
        { return  ( ( (*this)->read() ) ==  rhs_ ) ; }

    // SC_Register has implicit cast implemented so that
    // when it is used in a function argument, it returns its data value
    //operator const DATA_T &() { return (*this)->read(); }
    operator DATA_T &() { return (*this)->read(); }

    // overloaded equality '==' operator for SC_Register<DATA_T>
    //virtual bool operator == (const SC_Register<DATA_T>&)  const
    //    { return (*this)->read() ==  rhs_.read() ;}


};//class SC_reg_out_if_d


//==============================================================================
//  CLASS: port_SC_Stack_if
//==============================================================================

//! The specialized port for accessing SC_Stack_if_d methods
/*!
    Details here...
*/

class port_SC_Stack_if  :
    public sc_port<SC_Stack_if>
{
public:
    void            push        (unsigned int item) {(*this) -> push(item); }
    void            push_back   (unsigned int item) {(*this) -> push_back(item); }
    unsigned int    pop         ()                  { return (*this) -> pop(); }
    bool            empty       ()                  { return (*this) -> empty(); }
    unsigned int    size        ()                  { return (*this) -> size(); }

};//class SC_Stack_if

    //==============================================================================
    //  CLASS: port_SC_RequestTable_if
    //==============================================================================

    //! The specialized port for accessing SC_RequestTable_if methods
    /*!
        Details here...
    */

    class port_SC_RequestTable_if  :
        public sc_port<SC_RequestTable_if>
    {
    public:
        void            push        (const SBA::MemAddress address,SBA::Word item) {(*this) -> push(address,item); }
        unsigned int    shift         (const SBA::MemAddress address)                  { return (*this) -> shift(address); }
        unsigned int    size        (const SBA::MemAddress address)                  { return (*this) -> size(address); }

    };//class port_SC_RequestTable_if


    //==============================================================================
    //  CLASS: port_SC_LookupTable_if
    //==============================================================================

    //! The specialized port for accessing SC_LookupTable_if methods
    /*!
        Details here...
    */

    class port_SC_LookupTable_if  :
        public sc_port<SC_LookupTable_if>
    {
    public:

		void write(const SBA::Uint64 addr, SBA::Sint64 val)  { return (*this) -> write(addr,val); }
		SBA::Sint64 read(const SBA::Uint64 addr)  { return (*this) -> read(addr); }
		void erase(const SBA::Uint64 addr) { return (*this) -> erase(addr); }
		unsigned int size() { return (*this) -> size(); }
		unsigned int count(const SBA::Uint64 addr)  { return (*this) -> count(addr); }

    };//class port_SC_LookupTable_if

//==============================================================================
//  CLASS: port_SC_SubtaskList_if
//==============================================================================

//! The specialized port for accessing SC_SubtaskList_if methods
/*!
    ...
*/

class port_SC_SubtaskList_if  :
    public sc_port<SC_SubtaskList_if>
{
public:
    void                    lock() { (*this)->lock(); }
    void                    unlock() { (*this)->unlock(); }

    void                    add                 (const Subtask subtask)                             { (*this)->add              (subtask); }
    void                    remove              (const Subtask subtask)                             { (*this)->remove           (subtask); }
    Subtask_Argument_List&  arguments           (const Subtask subtask)                             { return((*this)->arguments (subtask) ); }
    void                    add_arg           (const Subtask subtask,MemAddress address)          { (*this)->add_arg        (subtask, address); }
    Subtask_Status          status              (const Subtask subtask)                             {
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" port_SC_SubtaskList_if:: get subtask " <<subtask <<" status : "<< (*this)->status(subtask) <<"\n";
#endif
            return((*this)->status    (subtask) );
    }
    void                    status              (const Subtask subtask,Subtask_Status status_)      {
    	(*this)->status           (subtask, status_);
#ifdef VERBOSE
            OSTREAM << std::setw(12) << setfill(' ') << sc_time_stamp() << ": "
            <<name()<<" port_SC_SubtaskList_if:: set subtask " <<subtask <<" status to "<<status_<<"\n";
#endif
    	}
    Word                    return_as           (const Subtask subtask)                             { return((*this)->return_as (subtask) ); }
    void                    return_as           (const Subtask subtask,Word return_as_)             { (*this)->return_as        (subtask, return_as_); }
    Symbol_t                called_as           (const Subtask subtask)                             { return((*this)->called_as (subtask) ); }
    void                    called_as           (const Subtask subtask,Symbol_t called_as)          { (*this)->called_as        (subtask, called_as); }
    Service                 to                  (const Subtask subtask)                             { return((*this)->to        (subtask) ); }
    void                    to                  (const Subtask subtask, Service to)                 { (*this)->to               (subtask, to); }
    Service                 return_to           (const Subtask subtask)                             { return((*this)->return_to (subtask) ); }
    void                    return_to           (const Subtask subtask, Service return_to)          { (*this)->return_to        (subtask, return_to); }
    Word                    ack_to              (const Subtask subtask)                             { return((*this)->ack_to    (subtask) ); }
    void                    ack_to              (const Subtask subtask, Word ack_to)                { (*this)->ack_to           (subtask, ack_to); }
    uint                    redir               (const Subtask subtask)                             { return((*this)->redir     (subtask) ); }
    void                    redir               (const Subtask subtask, uint redir)                 { (*this)->redir            (subtask, redir); }
    uint                    waiting_for_ack     (const Subtask subtask)                             { return((*this)->waiting_for_ack(subtask)); }
    void                    waiting_for_ack     (const Subtask subtask, uint waiting_for_ack)       { (*this)->waiting_for_ack  (subtask, waiting_for_ack); }
    CodeAddress             code_address        (const Subtask subtask)                             { return((*this)->code_address(subtask)); }
    void                    code_address        (const Subtask subtask, CodeAddress code_address)   { (*this)->code_address     (subtask, code_address); }
    uint                    service_id          (const Subtask subtask)                             { return((*this)->service_id(subtask) ); }
    void                    service_id          (const Subtask subtask, uint service_id)            { (*this)->service_id       (subtask, service_id); }
    uint                    nargs               (const Subtask subtask)                             { return((*this)->nargs     (subtask) ); }
    void                    nargs               (const Subtask subtask, uint val)                   { (*this)->nargs            (subtask, val); }
    uint                    nargs_absent        (const Subtask subtask)                             { return((*this)->nargs_absent(subtask) ); }
    void                    nargs_absent        (const Subtask subtask, uint val)                   { (*this)->nargs_absent     (subtask, val); }
    void                    decr_nargs_absent   (const Subtask subtask)                             { (*this)->decr_nargs_absent(subtask); }
    void                    incr_nargs_absent   (const Subtask subtask)                             { (*this)->incr_nargs_absent(subtask); }
    uint                    mode                (const Subtask subtask)                             { return((*this)->mode      (subtask) ); }
    void                    mode                (const Subtask subtask, uint val)                   { (*this)->mode             (subtask, val); }
    uint                    reg                 (const Subtask subtask)                             { return((*this)->reg       (subtask) ); }
    void                    reg                 (const Subtask subtask, uint val)                   { (*this)->reg              (subtask, val); }

    uint                    offset                 (const Subtask subtask)                             { return((*this)->offset       (subtask) ); }
    void                    offset                 (const Subtask subtask, uint val)                   { (*this)->offset              (subtask, val); }

    uint                    fsize                 (const Subtask subtask)                             { return((*this)->fsize       (subtask) ); }
    void                    fsize                 (const Subtask subtask, uint val)                   { (*this)->fsize              (subtask, val); }

    Subtasks                subtasks            ()                                                  { return((*this)->subtasks  () ); }
    Subtask_List_Item       entry               (const Subtask subtask)                             { return((*this)->entry     (subtask) ); }

}; //class: port_SC_SubtaskList_if

//==============================================================================
//  CLASS: port_SC_Config_if
//==============================================================================

//! The specialized port fo the SC_Config_if, to allow port-access to a SBA::Config object
/*!
    Details here...
*/

class port_SC_Config_if  :
    public sc_port<SC_Config_if>
{
public:
    SBA::ServicePair& read (SBA::Service service_id)
    {
        return (*this)->read(service_id);
    }

    // overloading the subscript '[]' operator
    SBA::ServicePair&   operator [] (SBA::Service service_id)
    {
        return (*this)->read(service_id);
    }

};//class port_SC_Config_if


//==============================================================================
//  CLASS: port_SC_Deque_if
//==============================================================================

//! The port_SC_Deque_if
/*!
    Details here...
*/
template <typename DATA_T>
class port_SC_Deque_if:
    public sc_port <SC_Deque_if<DATA_T> >
{
public:
    DATA_T          shift       ()              {return (*this) -> shift() ;}
    void            unshift     (DATA_T& item)  {(*this) -> unshift(item) ;}
    void            push        (DATA_T& item)  {(*this) -> push(item) ;}
    DATA_T          pop         ()              {return (*this) -> pop() ;}
    // *this is the pointer to interface object, so dereference it to use
    // its [] method (I know no way of using [] through ->)
    //DATA_T&         operator[]  (unsigned int i){return (*(*this))[i]; }
    DATA_T&         operator[]  (unsigned int i){return (*this) -> at(i) ; }
    DATA_T&         at          (unsigned int i) {return (*this) -> at(i) ; }
    unsigned int    length      ()              {return (*this) -> length();}
    unsigned int    size        ()              {return (*this) -> length();}
    void            clear       ()              {(*this) -> clear();}
    // read_all is needed in placed where complete data element needs to be read/written
    deque<DATA_T>&  read_all    ()              {return (*this) -> read_all();}
    operator Word_List& ()
    {
    	return (Word_List&)((*this) -> read_all());
    }
    // overloading the assignment operator for SC_Deque, so that it accepts deque<DATA_T> type
    // to enable initialization of local stl::deque object in one go
    void            operator = (const deque<DATA_T>& rhs_)
    { (*this)->read_all() = rhs_; }

};//class port_SC_Deque_if



}//namespace: SC_SBA




#endif /* SC_SPEC_PORTS_H_ */

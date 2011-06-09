#ifndef _SC_SERVICECORE_SIG_H_
#define _SC_SERVICECORE_SIG_H_
class SC_ServiceCore : public sc_module
{
public:
	// ---------------------------- PORTS --------------------------------------
    port_SC_SubtaskList_if              subtask_list;       //!< w/r port for subtask_list access
    port_SC_reg_if<SBA::Core_Status>    core_status;        //!< write/read port for code_status access
    port_SC_Deque_if<SBA::Word>         results_store;      //!< Fifo access to the results store
    port_SC_reg_if<bool>                ack_ok;             //!< access to ack_ok boolean
    port_SC_reg_if<SBA::Packet_Type>    core_return_type;   //!< ...
    port_SC_reg_if<SBA::Subtask>   current_subtask;
    port_SC_reg_if<uint>   n_args;
    port_SC_reg_if<uint>   opcode;
    port_SC_reg_if<Service>				service_id;			//!< service_id inside the service_manager
    port_SC_Deque_if<SBA::MemAddress>   arg_addresses;      //!< read write port for arg_addresses memory
    // is access to data_store required? Does not seem so from the code, but drawing on whiteboard indicates it is.
    port_SC_Memory_if <MemAddress, Data>  data_store;       //!< write/read port for data memory access
    port_SC_Memory_if <MemAddress, Data>  code_store;       //!< write/read port for data memory access
    port_SC_Config_if           		cfg;       //!< port for access to SBA::Config object (wrapped inside an sc_module)
    port_SC_Memory_if <MemAddress,SBA::Word> symbol_table;      //!< port for access to symbol_table

    port_SC_Fifo_if<Packet_t>    subtask_reference_fifo;//!< read port for posting straight into subtask_reference_fifo
    port_SC_Fifo_if<Packet_t>    transceiver_tx_fifo;//!< read port for posting straight into subtask_reference_fifo
    port_SC_Stack_if			data_address_stack;
    port_SC_LookupTable_if		lookup_table;
    // ---------------------------- LOCALS -------------------------------------
    SBA::Core_Status local_core_status;
    SBA::MemAddresses arg_addresses_local;
	SBA::Service service;
	SBA::State_Register state_register;
	// ---------------------------- METHODS ------------------------------------
	void do_proc();

    // overload sc_module.kind() to return a more appropriate class name
    virtual const char* kind() const
        { return "SC_service_core"; }
	// ---------------------------- CONSTRUCTOR --------------------------------
	SC_HAS_PROCESS(SC_ServiceCore);
	SC_ServiceCore(sc_module_name nm,
			SBA::Service        s_
			) : sc_module(nm),
				service(s_)
	{
		SC_THREAD(do_proc);

        // creation debug message..
        const_debug_msg(name(),kind());
	}
private:
};//class: SC_service_core
#endif // #ifndef _SC_SERVICECORE_SIG_H_

/*
********************************************************************************
                 |
  File Name      | SC_Config.h
-----------------|--------------------------------------------------------------
  Project        | SystemC Model of GANNET Hardware
-----------------|--------------------------------------------------------------
  Created        | 04-Jan-2009. Computing Science, University of Glasgow
-----------------|--------------------------------------------------------------
  Description    | The SystemC wrapper for SBA::Config class
-----------------|--------------------------------------------------------------
  Modifications  |
                 |
********************************************************************************

  (c) 2008-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>, Syed Waqar Nabi
    
  */

#ifndef SC_CONFIG_H_
#define SC_CONFIG_H_

#include "SC_sba.h"

using namespace std;

//==============================================================================
//  CLASS: SC_Config_if
//==============================================================================



//==============================================================================
//  CLASS: SC_Config
//==============================================================================

//! The namespace for the SystemC Model of Gannet SBA
namespace SC_SBA {

//! This is a systemc wrapper class around an SBA::Config object
    /*!
       A SystemC wrapper class is needed for the config object to allow
       it to be accessed through ports
       It just makes things cleaner.
       In terms the actual hardware desing, this has no real significance
       In systemc context, it is a hierarchichal channel, and exorts the SC_Config_if
    */
class SC_Config :
    public sc_module    ,
    public SC_Config_if

{
public:
    // ---------------------------- PORTS --------------------------------------
    sc_export<SC_Config_if > xp_1; // Export for access to Config object
    sc_export<SC_Config_if > xp_2; // Export for access to Config object

    // ---------------------------- Sub-Modules --------------------------------
    Config cfg;  //!< the local SC_SBA::Config object around which this SystemC class is wrapped
                //!< Note that this is not the C++ Config object, but specialized one for SystemC

    // ---------------------------- METHODS ------------------------------------
    // The Config object contains a map of <Service, ServicePair>
    // which is called 'services'
    // this read function accepts  key to this map (of type ~Service) and returns a REFERENCE
    // to the value )of type ServicePair
    // the reading entity can then do what it wants with it.
    SBA::ServicePair& read (SBA::Service service_id) { return  cfg.services[service_id];};

    // overloading the subscript '[]' operator to give read and write access
    // note that returning by reference, so that it can be used to write as well
    SBA::ServicePair&  operator [] (SBA::Service service_id) { return  cfg.services[service_id];}

    virtual const char* kind() const
        { return "SC_Config"; }

    // ---------------------------- CONSTRUCTOR --------------------------------
    SC_HAS_PROCESS(SC_Config);
    SC_Config(sc_module_name nm):
        sc_module(nm)
    {
        // Bind exports to *this, which is a hierarchichal channel that implements the SC_Config_if interface
        xp_1.bind(*this);
        xp_2.bind(*this);

        // creation debug message..
        const_debug_msg(name(),kind());
    }
};/* class: SC_Config */


/*
//------------------------------------------------------------------------------
//  SC_Config :: read()
//------------------------------------------------------------------------------
SBA::ServicePair& SC_Config :: read(SBA::Service service_id)
{
    // return the ServicePair value that corresponds to the passed Service key
    return  cfg.services[service_id];
}// funct: SC_Config :: read()
*/


} /* namespace: SC_SBA */



#endif /* SC_CONFIG_H_ */

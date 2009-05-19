# LookupTable.rb
#   
# :title: Service-based SoC project - Transmitter/Receiver class
#
#--
#
#/* ***** BEGIN LICENSE BLOCK *****
# * Version: AFL 2.1
# *
# * The contents of this file are subject to the Academic Free License Version
# * 2.1 (the "License") you may not use this file except in compliance with
# * the License. You may obtain a copy of the License at
# * http://opensource.org/licenses/afl-2.1.php
# *
# * Software distributed under the License is distributed on an "AS IS" basis,
# * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
# * for the specific language governing rights and limitations under the
# * License.
# *
# *  (c) 2004-2005 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
#
#//==============================================================================
#//
#// Service-based SoC project - LookupTable class 
#//
#//==============================================================================
#
#// $Id: LookupTable.rb 2155 2009-01-28 11:39:41Z socgroup $
#++


class SBA_LookupTable

    def initialize
        @lookup={}
    end
    
    def write(k,v)
        @lookup[k]=v
    end
    
    def read(k)
         return @lookup[k]
    end
    
    def erase(k)
        @lookup.delete(k)
    end        
    
    def size()
        return @lookup.length
    end
    
    def count(k)
        return @lookup.has_key?(k)?1:0
    end

end # of SBA_LookupTable class

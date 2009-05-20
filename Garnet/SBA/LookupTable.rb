# LookupTable.rb
#   
# :title: Gannet Service-based SoC project - Transmitter/Receiver class
#
#--
#
# *
# *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
# *  
#
#//==============================================================================
#//
#// Gannet Service-based SoC project - LookupTable class 
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

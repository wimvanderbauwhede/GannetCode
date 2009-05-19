#/** SBA_Debug
#   
# :title: Garnet Service-based SoC project - SBA Debug Support
#
#*/
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
# *  (c) 2004-2005 Wim Vanderbauwhede <wimdcs.gla.ac.uk>
# *  
# *
# * ***** END LICENSE BLOCK ***** */
#
#
#// $Id: Debug.rb 2155 2009-01-28 11:39:41Z socgroup $ 


module SBA_Debug

    def SBA_Debug.pp(try_array)
        str="PRETTY-PRINT FAILED!"
        if try_array.is_a?(Array)
            if try_array.length==1
                try_val=try_array[0]
                 if not try_val.is_a?(SBA_Symbol) and ((try_val.is_a?(Integer) and NUM==1)  or (try_val.is_a?(String) and NUM==0))
                try_symbol=SBA_Symbol.new(try_val)
                if try_symbol.Kind==0 and try_symbol.Datatype==0
                    str= "#{try_val}"
                else
                    str="#{try_symbol}"
                end
                else
                    str= "#{try_val}"
                    end
            else
                str=""
                for elt in try_array
                    str+= "#{elt}\n"
                end
            end
        else # not Array
            try_val=try_array
            if not try_val.is_a?(SBA_Symbol) and ((try_val.is_a?(Integer) and NUM==1) or (try_val.is_a?(String) and NUM==0))
            try_symbol=SBA_Symbol.new(try_val)
            if try_symbol.Kind==0 and try_symbol.Datatype==0
                str= "#{try_val}"
            else
                str="#{try_symbol}"
            end
            else
                str= "#{try_val}"
                end
        end
        return str
    end
    
    def pretty(str)
    return SBA_Debug.pp(str)
    end
end # of SBA_Debug
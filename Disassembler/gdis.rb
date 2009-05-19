#
# :title: Garnet Service-based SoC project - Gannet disassembler
##
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
# $Id$

WORDSZ=32
NUM=1
TO_YAML=0
VERBOSE=1

if ENV.has_key?('GANNET_DIR')
    $LOAD_PATH.push("#{ENV['GANNET_DIR']}/Garnet/")
else    
    $LOAD_PATH.push("#{ENV['HOME']}/SoC_Research/Code/Gannet/Garnet/")
end

help= <<EOH
    Usage: gdis [-h] .tdc file 
EOH

require 'optparse'

opts=OptionParser.new
opts.on("-v","--verbose") {}
FP=0
opts.on("-f","--float-only") {FP=1}
opts.on("-h","--help") {
puts help
exit
}

tdc_file=opts.parse(ARGV).join('')

if tdc_file != 'NONE'
    require 'SBA/SystemConfiguration.rb'
    require 'SBA/ServiceConfiguration.rb'
    require 'SBA/TaskDescription.rb'
    td=SBA_TaskDescription.new(tdc_file,0)
end



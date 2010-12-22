#!/usr/bin/env ruby 
# simple script to convert a bytecode file into a static array of 32-bit Words
NBYTES=4
tdc_file=ARGV[0]

def read_bytecode(tdc_file) #t Bytecode (string)
#        puts "read_bytecode()"
    tdch=File.open(tdc_file,"r") #C++ FILE * fd=fopen(tdc_file.c_str(),"r");
    bytewords=[] #C++ Bytecode bytewords;
    byteword=0 #t Word
    hwb=0 #t uint
    #C++ int byte=0; 
    #C++ while(byte!=EOF) {
    tdch.each_byte {|byte| #C++ byte=fgetc(fd);  
        byteword+=(byte<<(8*(NBYTES-1-hwb)))
        hwb=hwb+1
        if hwb==NBYTES
            hwb=0
            bytewords.push(byteword) #s/push/push_back/
            byteword=0
        end            
    } #C++ }
#        puts "exit read_bytecode()"
    return bytewords
end 

word_list=read_bytecode(tdc_file)
nwords=word_list.length
puts "namespace SBA {"
puts "uint nwords=#{nwords};"
puts "Word byc[#{nwords}]={"
last_word=word_list.pop
for word in word_list
puts sprintf("0x%08xUL,",word)
end
puts sprintf("0x%08xUL",word)
puts "};"
puts "} // namespace SBA"

require 'socket'
arg=ARGV.shift
clnt=TCPSocket.new('127.0.0.1',7188)

puts "<#{arg}>"    
clnt.puts(arg)
clnt.close()

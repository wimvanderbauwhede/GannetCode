require 'socket'

svr=TCPServer.new('127.0.0.1',7188)

    puts "Accepting connections..."
    conn=svr.accept()
    
    puts "Starting to read"
    msg=conn.read()
    puts "<#{msg}>"
puts "exit"

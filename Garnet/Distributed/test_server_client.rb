require 'socket'
# I guess the best thing to do is to fork off a separate server, this will guaranteed not block.
# But will it be ready in time?
svr=TCPServer.new('127.0.0.1',7188)
#clnt=TCPSocket.new('127.0.0.1',7188)
launch=0
loop do
    puts "Accepting connections..."
    begin
        conn=svr.accept_nonblock() 
puts "non-blocking!"        
        if (launch==0)
    launch=1
    puts "Forking clients"
    for i in 1..10
    system("ruby test_client.rb #{i} &")
    end
    end
#    end
#    puts "Forking client"
#    system("ruby test_client.rb")
#    clnt.puts("Hello!")
        puts "Starting to read"
#        for j in 1..30
#        begin
        msg=conn.read()
#    rescue Errno::EAGAIN
#    retry
#    end
        puts "<#{msg}>"
#        conn.close()
        rescue Errno::EAGAIN
    IO.select([svr])
retry
            end
end
puts "End of thread"


#end
puts "exit"

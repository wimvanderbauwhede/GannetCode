; The idea is that every service can have a few local variables, what this
really means is that the services caches the result of a computation.
This sounds good but there are many issues. 
1. Who does the caching?
(S1 (local z (S2 ...))
    z
    (S3 ... z ...)
    )
1.1 If S1 caches the computation (S2 ...) then we must have a lookup mechanism
that can wait, similar to (read ...). The 'bare' z in S1 is not an issue. But
what about the z in (S3...)? 
We must encode it as service:subtask:arg , e.g. S1:i:1. Then a request packet
will find the actual address of z via the subtask list entry (i) and the
argument (1). But what if the result is not yet there? The request must wait.
How? We could have a "local requested" flag on the argument; but then the
information of the caller (i.e. the Return-as) must be stored. 
* Normally, S3 would send a ref packet to S2. This would activate the subtask j at S2 and the
subtask list item would contain the Return-as
* With locals, S3 will send a req packet to S3. In principle, S3 could receive
 a very large number of req packets. So I gess we need additional
 infrastructure: a table of the requests and their status, or simply a fifo
 with the requests per local. The easy way is to restrict the number of locals
 to 4 and the number of requests per local also to 4. Then we simply have 4
 fifo queues of max. 4 words, i.e. 16 words. We can use the Kind field as status
 field. But even so, that's 16 words per subtask, which is the dominant
 component in the subtask list item. And that only because we need a way to
 wait - and we can't use quotes!

1.2  Now, if S2 would cache the computation, the waiting issue is the same. The
 lookup scheme is different: the result is looked up using S2:j, i.e. this is
 just like LABEL but instead of running the task we request the cached value.
 Note that in this case S1 does not need to store a result for the
 computation, dispatch & forget.
 The main improvement is that S2 can get 4 requests per cached task instead of
 16. So this scheme seems to have most merit.
2. How do we implement this?
2.1 Compiler
* The compiler must now consider _every_ service as a scope. 
* The LOCAL (or CACHE?) "calls" are treated like ASSIGN as far as binding is
concerned and as LABEL as far as removal is concerned. 
* The effect of the LOCAL modifier is a new kind: K_C. I think I will abuse
the Quoted field to indicate if a K_C reference should be activated or the
cached result requested. Let's say that a quoted K_C is a request.
2.2 Service Manager
The Service Manager can not not clean up a task after it finishes the
computation. Insteas, the result of the task must be stored "somewhere".
We immediately see a big issue as this means allocating space for every
subtask or have a mechanism to use the data storage - but that will only work
if the size of the result is the same as the size of the arguments. 
Not that scheme 1.1 does not have that issue.
I guess the pragmatic approach is to limit the number of "concurrent locals".
Then we can have a small cache memory dedicated to this, with its own stack. 

A key issue is of course cleaning-up: when the local goes out of scope it must
be cleaned up. So S1 should really notify S2 that it can clean up. This is a
weak point. With scheme 1.1 it is a lot easier. Because now it means that the
Service Manager must keep track of the locals (that is easy: they are the
first args), and on exit it should send a clean-up packet, i.e. an ACK, to
every local, so at most 4 ACKs. That is possible but somewhat slow.

Still, scheme 1.2 seems to be preferred: I like the idea of a cache memory and
we already have an ACK mechanism, it is nice to see that it can naturally work
to clean up the cache.






        (+
            (local x (* (local z (+ '2 '4))  '2 z '3))
            (local y (- '5 '2))
            (* (local v (* '6 '7)) x (+ y v))
            y
            )
            

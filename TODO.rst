1. provide json output of the data from feeding a list of ed2k hashes to anidb and/or files
2. look into some sort of local caching
3. look into a simple console interface in which i type in request and it
   return with replies for testing


Design:
1. plain library, no need to do event based for most parts, only the reciever
2. general api can be:
    connect
    send (message)
    recieve (message) - blocking
    processTimeouts - for processing timeouts on the state table
    disconnect

3. When you send a packet, generate a tag, enroll the tag, sending information, timestamp into a state table
4. when you recieve a packet, parse and consult the state table
5. deal with timeouts via checking the statetable and expiring/calling back stuff to deal with timeouts

In general can probably do most of this without threading, however, it depends on how i deal with the state table, may be
worth it to thread the send/recieve so that it can properly lock/block the mvar for state table.

Anyway the recieving will be blocking. so might want to like look into a simple "chain spawn" or hook for hooking the reciever
into a event loop.

by thread chain, i mean it spawns a thread, blocks till it recieves something, spawn a new thread, which then blocks, and proceed to
deal with the data that it has recieved.




- Client library design.
    - compression - enabled by default
    - encoding    - utf8 enabled by default
    - encryption  - optional (user)
    - session     - Enabled by default
    - Tag<->thread/send/reciever
    
    - These above is what the client library is responsible for

    - The underlaying udp layer is resposible for regulating the 
      sending rate of the packets


- Now how do we deal with recieving udp packets and assigning it to the
  proper thread to wake up and proceed?

    - Each thread will "add its tag + send data" to a table which is
      protected by a mvar then it will send the data over the pipe, then
      poll the send table for updates, it will check if its its own, if
      not it will then block again

    - We have a master thread/loop for recieving udp packets, it will
      recieve, parse, then look up the tag (if any) in the send table
      and then update it which then wake up the waiting threads, in
      which they poll it to see if its their data.

    - Now for data without tags, we can special handle these data, because
      they will need special handling


-- Better solution is. a table with thread id, tag, data, then i recieve
the data and then select the right thread to wake up in which it fetches
the data and proceed on


for example:

mvar [ (tag, mvar ParsedData) ]

Then a thread basially generates a tag, and a mvar and put it into the 
list, then it sends the message and takes from the mvar parseddata and blocks
till the reciever thread gets the data and parse + match it to the tag and
put it into the mvar table

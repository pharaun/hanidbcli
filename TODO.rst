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

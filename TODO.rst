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






NEXT PHASE:

Implement a hashing service (request file to hash, poll for status, retrieve hash)

Implement a client/server arch with hasher/network/anidb in the daemon then a git alike cli interface for interacting with the service

eventually implement a file move/rename service for cleaning up the files/etc

make sure to do this in a secure manner, such as dropping privs to the lowest, restrict to trusted ip, etc...



READONLY IO:
19:53:07 < JoeyA> You don't even need to define a type.
19:53:20 < JoeyA> class MonadRIO m where readFile :: ...
19:53:21 < pharaun> is this a good thing to do or? I'm just thinking that it would be nice from a type pov to prove/define a read only io
19:53:36 < JoeyA> then instance MonadRIO IO where readFile = Prelude.readFile; ...
19:54:05 < JoeyA> err, class Monad m => MonadRIO m
19:54:34 < JoeyA> Then: foo :: MonadRIO m => FilePath -> m String; foo = ...
9:55:07 < JoeyA> Even though foo is instantiated with IO, its type guarantees that it will only perform the actions listed in the MonadRIO class.
19:55:46 < johnw> neat try, JoeyA
19:55:48 < johnw> trick
19:55:48 < JoeyA> pharaun: Well, hGetLine, for example, is not read-only, since it modifies the state of the Handle.
19:56:08 < pharaun> JoeyA: ByteString hGet?
19:56:24 < JoeyA> If it advances the stream position, it's a modification.
19:56:24 < ddarius> Reading memory is, in general, not read-only.
19:56:35 < pharaun> i'm just trying to define/figure out how to limit the scope of IO action
19:56:50 < JoeyA> pharaun: What's the bigger picture?
19:57:14 < pharaun> a file hasher, i give it a list of paths
19:57:25 < nu11ptr> anybody want to answer a quick q on state monad?
19:57:27 < ddarius> Really this is just a naming thing.  JoeyA's approach will work with a variety of restrictions and their combinations.
19:57:27 < JoeyA> Though I suppose "What's the bigger picture" is a warning sign that modularity is about to be broken.
19:57:29 < pharaun> and it hashes the file and go into the directory's children and hash those file
19:57:54 < pharaun> and it seems to make sense to restrict that part of the code base to a read only io of some form
19:58:04 < pharaun> beacuse it shouldn't be like opening a file and writing something for ex
19:58:17 < pharaun> the fact that its in IO gives it the ability to do just that
19:58:35 < JoeyA> pharaun: You could just define a monad class like I described, so you can list exactly what actions the code is allowed to perform.
19:58:42 < JoeyA> You could even use this for mocking, for example.
19:58:57 < pharaun> JoeyA: yeah i was starting to wonder how i was going to test some of these IO related submodules
19:59:00 < ddarius> Yes, this would be convenient/useful for more than just restricting the code.
19:59:15 < pharaun> so if i can do this then mock out the IO part and use that for testing, even better
19:59:25 < ddarius> It would also allow you to seamlessly generalize to monad transformer stacks or other non-IO monads.
20:00:59 < ddarius> It would be extremely useful to factor various IO operations into various classes and have many functions written in terms of these methods rather than directly against their archetypical IO instantiations.
20:01:28 < pharaun> ddarius: i was hoping to eventually do that, like haveing NIO (network io), (rio) read only io, etc
20:01:53 < pharaun> so i can better isolate parts of the code and take better advantage of monad stack/transformers
20:01:53 < cmccann> an approach I am somewhat fond of is to have no class constraint other than Monad itself, and instead pass in arguments (or use ReaderT) to provide appropriate operations.
20:02:19 < cmccann> that restricts such code to the operations you hand it at run time, as well as making it easier to swap out the choice of monad for e.g. testing purposes.
20:02:40  * ddarius would be perfectly happy with that too.
20:02:54 < ddarius> The real issue we have is recreating all the IO-only libraries.



import Prelude hiding (readFile)
import qualified Data.ByteString.Lazy as L

data Entry = Entry SHA1 FilePath

data FileType = File | Directory

class Monad m => MonadHash m where
    getFileType          :: FilePath -> m FileType
    readFile             :: FilePath -> m L.ByteString
    getDirectoryContents :: FilePath -> m [FilePath]
    yield                :: Entry -> m ()

traverseDirectory :: MonadHash m => m ()

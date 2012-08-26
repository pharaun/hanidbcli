module Anidb.Parse.Reply
    ( parseAniReply
    , AniReplyParsed

    -- TODO: may want to remove these?
    , AniReply
    , ParseError

    , getSession
    , getTag
    ) where

import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
import qualified Codec.Binary.UTF8.String as U
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as L

-- Types
data AniReply = AniReply {
    hTag :: Maybe String
    , hReturnCode :: Integer

    , hMessage :: String
    -- 555 BANNED {str reason}, Error string, etc...
    , hExtraMessage :: Maybe String

    , hData :: Maybe AniHeaderData

    -- TODO: Add a more detailed type, for now dump data in here as a string
    , bData :: Maybe String
    } deriving (Show)

-- TODO: can add 2 more HeaderData (Buddy list stuff, and notification)
data AniHeaderData =
    AniHeaderSalt String |
    AniHeaderVersion String |
    AniHeaderUptime Integer |
    AniHeaderPort Integer |
    AniHeaderSession {
        adSession :: String
        , adIP :: Maybe String
        , adPort :: Maybe Integer
        , adImgSrv :: Maybe String
    }
    deriving (Show)

-- TODO: Not sure yet
type AniReplyParsed = Either ParseError AniReply


-- Data extraction
getSession :: AniReplyParsed -> Maybe String
getSession (Left _)  = Nothing
getSession (Right x) = getHdrSes (hData x)
    where
        getHdrSes (Just AniHeaderSession{adSession=x}) = Just x
        getHdrSes _ = Nothing

getTag :: AniReplyParsed -> Maybe String
getTag (Left _)  = Nothing
getTag (Right x) = hTag x


-- Test parser wrapper for dealing with compressed data,
-- still need to deal with encrypted data and better error handling
parseAniReply :: L.ByteString -> AniReplyParsed
parseAniReply inputData =
        parse anidbReply "(unknown)" $ U.decode $ L.unpack input
    where
        input = if L.take 2 inputData == L.pack [0,0] then Z.decompress $ L.drop 2 inputData else inputData


--
-- Sample data:
--
-- *AniNetwork> version b
-- "998 VERSION\n0.03.705 (2012-08-12)\n"
--
-- *AniNetwork> uptime b
-- "501 LOGIN FIRST\n"
--
-- *AniNetwork> ping b True
-- "300 PONG\n39728\n"
--
-- Protocol defination:
--
-- opt      - optional
-- int2     - 2 byte Integer (in string representation)
-- int4     - 4 byte Integer (in string representation)
-- str      - String (UDP packet length restricts string size to 1400 bytes)
-- hexstr   - a hex representation of a decimal value, two characters per byte.
-- boolean  - true or false - use '1' for true, '0' for false
--
-- Header:
-- {opt compressed int2 (00)}
-- {opt tag str}
-- {three digit return code (int3)}
-- {opt session key (str, 4-6 char)}
-- {opt str ip}:{opt int2 port}
-- {opt str salt}
-- {str return string}
-- {opt error message str}\n
--
-- Sample (assuming all can have compression and tag) header:
-- 200 {str session_key} LOGIN ACCEPTED
-- 201 {str session_key} LOGIN ACCEPTED - NEW VERSION AVAILABLE
--
-- With NAT enabled - (ipv4 for now...)
-- 200 {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED
-- 201 {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED - NEW VERSION AVAILABLE
--
-- With imgServer enabled - Context/connection dependent
-- 200 {str session_key} LOGIN ACCEPTED
-- {str image server name}
-- 201 {str session_key} LOGIN ACCEPTED - NEW VERSION AVAILABLE
-- {str image server name}
--
-- 500 LOGIN FAILED
-- 504 CLIENT BANNED - {str reason}
-- 209 {str salt} ENCRYPTION ENABLED
--
-- Sample headers with known no data section, thus can parse next line:
-- 555 BANNED
-- {str reason}
--
-- 998 VERSION
-- {str server version}
--
-- 208 UPTIME
-- {int4 udpserver uptime in milliseconds}
--
-- 300 PONG
-- {int4 port} (when nat=1)
--
-- 6xx INTERNAL SERVER ERROR
-- ERROR: {str errormessage}
--
-- 6xx INTERNAL SERVER ERROR - {str errormessage}
--
-- Probably unsupported headers:
-- 253 {int2 start} {int2 end} {int2 total} BUDDY LIST
-- 254 {int2 start} {int2 end} {int2 total} BUDDY STATE
-- 720 {int4 notify_packet_id} NOTIFICATION - NEW FILE
-- 794 {int4 notify_packet_id} NOTIFICATION - NEW MESSAGE
--
--
-- Data section:
-- {data field 0}|{data field 1}|...|{data field n}
--

-- Anidb reply is composited of a header and in some case follow on data
anidbReply :: GenParser Char st AniReply
anidbReply = do
    (aTag, return_code) <- tagAndReturnCode
    (returnString, extraMessage, aniExtraData) <- remainingHeaders return_code
    eoh
    bodyData <- optionMaybe (many1 anyToken)
    eof

    return $ AniReply aTag return_code returnString extraMessage aniExtraData bodyData


tagAndReturnCode :: GenParser Char st (Maybe String, Integer)
tagAndReturnCode = try (do
        tag <- many1 (noneOf " ")
        skipMany space
        rc <- returnCode
        return (Just tag, rc))
    <|> (do
            rc <- returnCode
            return (Nothing, rc))


returnCode :: GenParser Char st Integer
returnCode = do
    ds <- count 3 digit
    return $ read ds


remainingHeaders :: Integer -> GenParser Char st (String, Maybe String, Maybe AniHeaderData)
remainingHeaders 200 = loginString
remainingHeaders 201 = loginString
remainingHeaders 208 = infoString 208
remainingHeaders 209 = encryptionString
remainingHeaders 300 = infoString 300
remainingHeaders 555 = internalError
remainingHeaders 998 = infoString 998
remainingHeaders n
         | n > 599 && n < 700 = internalError
         | otherwise          = defaultStringWrapper


-- 998 VERSION
-- {str server version}
--
-- 208 UPTIME
-- {int4 udpserver uptime in milliseconds}
--
-- 300 PONG
-- {int4 port} (when nat=1)
infoString :: Integer -> GenParser Char st (String, Maybe String, Maybe AniHeaderData)
infoString return_code = do
    skipMany space
    (msg, extraMsg) <- defaultString
    -- Mandatory/optional data
    hdata <- headerData

    case return_code of
        208 -> return (msg, extraMsg, Just $ AniHeaderUptime $ read $ fromJust hdata)
        300 -> return (msg, extraMsg, handlePong hdata)
        998 -> return (msg, extraMsg, Just $ AniHeaderVersion $ fromJust hdata)
    where
        handlePong :: Maybe String -> Maybe AniHeaderData
        handlePong Nothing  = Nothing
        handlePong (Just a) = Just $ AniHeaderPort $ read a


-- 209 {str salt} ENCRYPTION ENABLED
encryptionString :: GenParser Char st (String, Maybe String, Maybe AniHeaderData)
encryptionString = do
    skipMany space
    salt <- many1 (noneOf " ")
    (msg, extraMsg) <- defaultString
    return (msg, extraMsg, Just $ AniHeaderSalt salt)


-- 20[01] {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED - (NEW CLIENT AVAILABLE)
-- {str image server name}
loginString :: GenParser Char st (String, Maybe String, Maybe AniHeaderData)
loginString = do
    skipMany space
    session <- many1 alphaNum
    skipMany space

    -- the ip:port is optional
    (ip, port) <- ipPort

    skipMany space
    (msg, extraMsg) <- defaultString

    -- Optional img server
    imgsrv <- headerData

    return (msg, extraMsg, Just $ AniHeaderSession session ip port imgsrv)


headerData :: GenParser Char st (Maybe String)
headerData = try (do
        char '\n'
        srv <- many1 (noneOf "|\n")
        return $ Just srv)
    <|> return Nothing


-- Parse the ip:port out otherwise return empty strings
ipPort :: GenParser Char st (Maybe String, Maybe Integer)
ipPort = try (do
        ip <- many1 (noneOf ":")
        char ':'
        port <- many1 (noneOf " ")
        return (Just ip, Just $ read port))
    <|> return (Nothing, Nothing)


-- 555 BANNED
-- {str reason}
--
-- 6xx INTERNAL SERVER ERROR - {str errormessage}
-- 6xx INTERNAL SERVER ERROR
-- ERROR: {str errormessage}
internalError :: GenParser Char st (String, Maybe String, Maybe AniHeaderData)
internalError = do
    skipMany space
    msg <- many1 (noneOf "-\n")

    err <- try (char '-' >> skipMany space >> many1 (noneOf "\n"))
           <|> (char '\n' >> many1 (noneOf "|\n"))

    return (trimRight msg, Just err, Nothing)


-- 500 LOGIN FAILED
-- 504 CLIENT BANNED - {str reason}
defaultString :: GenParser Char st (String, Maybe String)
defaultString = do
    skipMany space
    msg <- many1 (noneOf "-\n")
    extraMsg <- optionMaybe (char '-' >> skipMany space >> many1 (noneOf "\n"))
    return (trimRight msg, extraMsg)


defaultStringWrapper :: GenParser Char st (String, Maybe String, Maybe AniHeaderData)
defaultStringWrapper = do
    (msg, extraMsg) <- defaultString
    return (msg, extraMsg, Nothing)


-- Supporting code
trimRight :: String -> String
trimRight str | all isSpace str = ""
trimRight (c : cs) = c : trimRight cs

eoh :: GenParser Char st Char
eoh = char '\n' <|> char '|'


-- TODO: parse the data
--
-- 22:44:40 < edwardk> pharaun: example;  data Foo = Foo { _fooFlags :: Int64 }; makeLenses ''Foo; flag31 = fooFlags.bitAt 31
-- 22:44:49 < edwardk> myFoo^.flag31
-- 22:44:52 < edwardk> (using lens)
-- 22:44:56 < edwardk> but you don't need lenses for this
-- 22:45:34 < pharaun> that would work for getting the value of what each flag is, ie 0 vs 1
-- 22:45:58 < edwardk> flag31 .~ True $ myFoo -- will let you update it as well
-- 22:46:07 < pharaun> but in this case say i send "0b1010" as the flag, i will get back "data1|data3"
-- 22:47:10 < edwardk> so what is the problem with that?
-- 22:47:26 < pharaun> that will be useful for setting/getting the flag value :) i'll save that to my todo, but how would i like map between the flag and the 
--      data in a useful manner. i was going to just bit shift through the flag - 0b1010, and return a list like [data1, Nothing, data3, Nothing[
-- 22:47:35 < edwardk> IntMap YourData  pass around the data in that
-- 22:48:07 < pharaun> oh so i pass around the returned data, then the flag, and do that kind of lens/map
-- 22:48:11 < pharaun> to map from the flag to the data
-- 22:48:19 < edwardk> yes
-- 22:48:26 < edwardk> no 56 argument constructor need apply
-- 22:48:30 < pharaun> yes
-- 22:48:33 < pharaun> that's much better idea
-- 22:49:14 < edwardk> you can read the elements of the map using the map lookup functions or Data.IntMap.Lens
-- 22:49:16 < edwardk> e.g.
-- 22:49:22 < edwardk> myMap^.at 31
-- 22:49:30 < edwardk> will give you Nothing or Just the data
--

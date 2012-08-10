-- Basic Anidb parser
import Data.String.Utils (strip)
import Text.ParserCombinators.Parsec
import qualified Codec.Binary.UTF8.String as U
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B


parseAnidb :: B.ByteString -> Either ParseError [[String]]
parseAnidb inputData =
        parse anidbReply "(unknown)" $ U.decode $ B.unpack paddedInput
    where
        input = if (B.take 2 inputData == B.pack [0,0]) then (Z.decompress $ B.drop 2 inputData) else (inputData)
        -- TODO: This is not very nice, proably nicer if we just flatout append newline, and have parser skip/nom on newline
        paddedInput = if ((B.pack [10]) `B.isSuffixOf` input) then (input) else (input`B.append` (B.pack [10])) -- [10] == \n

testString :: String
testString = "dadsf 201 JasdSf 10.0.1.123:233 LOGIN ACCEPTED - NEW VERSION AVAILABLE\nanidb.imgserver.com|testdata|dadf\n"

testEncode :: String -> B.ByteString
testEncode = B.pack . U.encode

testComp :: String -> B.ByteString
testComp input = B.pack [0,0] `B.append` (Z.compress $ testEncode input)

-- Protocol defination
--
-- opt - optional
-- int2 - 2 byte Integer (in string representation)
-- int4 - 4 byte Integer (in string representation)
-- boolean - true or false - use '1' for true, '0' for false
-- str - String (UDP packet length restricts string size to 1400 bytes)
-- hexstr - a hex representation of a decimal value, two characters per byte.
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
-- 201 {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED - blah
--
-- With imgServer enabled - Context/connection dependent
-- 200 {str session_key} LOGIN ACCEPTED
-- {str image server name}
--
-- 201 {str session_key} LOGIN ACCEPTED - NEW VERSION AVAILABLE
-- {str image server name}
--
-- 500 LOGIN FAILED
--
-- 504 CLIENT BANNED - {str reason}
--
-- 209 {str salt} ENCRYPTION ENABLED
--
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
--
-- 6xx INTERNAL SERVER ERROR
-- ERROR: {str errormessage}
--
-- 6xx INTERNAL SERVER ERROR - {str errormessage}
--
--
-- Probably unsupported headers:
-- 253 {int2 start} {int2 end} {int2 total} BUDDY LIST
-- 254 {int2 start} {int2 end} {int2 total} BUDDY STATE
-- 720 {int4 notify_packet_id} NOTIFICATION - NEW FILE
-- 794 {int4 notify_packet_id} NOTIFICATION - NEW MESSAGE
--
--
--
-- Data section:
-- {data field 0}|{data field 1}|...|{data field n}
--

-- A reply is composited of a header and sometime follow on data
anidbReply :: GenParser Char st [[String]]
anidbReply = do
    result <- headers
    dataz <- many line
    eof
    return (result ++ dataz)

headers :: GenParser Char st [[String]]
headers = do
    (aTag, return_code) <- tagAndReturnCode
    returnString <- remainingHeaders return_code
    eoh
    return [[aTag], [show return_code], returnString]


tagAndReturnCode :: GenParser Char st (String, Integer)
tagAndReturnCode = try (do
        tag <- many1 (noneOf " ")
        skipMany space
        rc <- returnCode
        return (tag, rc))
    <|> (do
            rc <- returnCode
            return ("", rc))

returnCode :: GenParser Char st Integer
returnCode = do
    ds <- count 3 digit
    return $ read ds


remainingHeaders :: Integer -> GenParser Char st [String]
remainingHeaders 200 = loginString
remainingHeaders 201 = loginString
remainingHeaders 208 = infoString
remainingHeaders 209 = encryptionString
remainingHeaders 300 = infoString
remainingHeaders 555 = infoString
remainingHeaders 998 = infoString
remainingHeaders n
         | n > 599   = defaultString -- TODO: Implement a special 6xx handler
         | n < 699   = defaultString
         | otherwise = defaultString


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
infoString :: GenParser Char st [String]
infoString = do
    skipMany space
    msg <- defaultString
    -- Mandatory/optional data
    hdata <- headerData
    return $ msg ++ [hdata]


-- 209 {str salt} ENCRYPTION ENABLED
encryptionString :: GenParser Char st [String]
encryptionString = do
    skipMany space
    salt <- many1 (noneOf " ")
    msg <- defaultString
    return $ [salt] ++ msg


-- 20[01] {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED - blah
-- {str image server name}
loginString :: GenParser Char st [String]
loginString = do
    skipMany space
    session <- many1 alphaNum
    skipMany space

    -- the ip:port is optional
    (ip, port) <- ipPort

    skipMany space
    msg <- defaultString

    -- Optional img server
    imgsrv <- headerData

    return $ [session, ip, port] ++ msg ++ [imgsrv]

headerData :: GenParser Char st String
headerData = try (do
        char '\n'
        srv <- many1 (noneOf "|\n")
        return srv)
    <|> (return "")


-- Parse the ip:port out otherwise return empty strings
ipPort :: GenParser Char st (String, String)
ipPort = try (do
        ip <- many1 (noneOf ":")
        char ':'
        port <- many1 (noneOf " ")
        return (ip, port))
    <|> (return ("", ""))


-- 500 LOGIN FAILED
--
-- 504 CLIENT BANNED - {str reason}
-- 6xx INTERNAL SERVER ERROR - {str errormessage}
defaultString :: GenParser Char st [String]
defaultString = do
    skipMany space
    foo <- (many1 (noneOf "-\n")) `sepBy1` string "-"
    return $ strip `map` foo


eol :: GenParser Char st Char
eol = char '\n'

eoh :: GenParser Char st Char
eoh = eol <|> char '|'

line :: GenParser Char st [String]
line = do
    result <- item
    eol
    return result

item :: GenParser Char st [String]
item = do
    first <- itemContent
    next <- remainingItem
    return (first : next)

remainingItem :: GenParser Char st [String]
remainingItem =
    (char '|' >> item)
    <|> (return [])

itemContent :: GenParser Char st String
itemContent = many (noneOf "|\n")



-- Currently unused
ip :: GenParser Char st String
ip = do { a1 <- decOctet; char '.'
        ; a2 <- decOctet; char '.'
        ; a3 <- decOctet; char '.'
        ; a4 <- decOctet
        ; return $ a1++"."++a2++"."++a3++"."++a4
        }

decOctet :: GenParser Char st String
decOctet = do
    a1 <- many1 digit
    if (read a1 :: Integer) > 255 then
        fail "Decimal ocet value too large"
    else
        return a1

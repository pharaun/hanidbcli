-- Basic Anidb parser
import Text.ParserCombinators.Parsec

parseAnidb :: String -> Either ParseError [[String]]
parseAnidb input = parse anidbReply "(unknown)" input

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
-- 500 LOGIN FAILED 
--
-- 504 CLIENT BANNED - {str reason}
-- 6xx INTERNAL SERVER ERROR - {str errormessage} 
--
-- 209 {str salt} ENCRYPTION ENABLED 
--
-- 200 {str session_key} LOGIN ACCEPTED
-- 201 {str session_key} LOGIN ACCEPTED - NEW VERSION AVAILABLE 
--
-- With NAT enabled - (ipv4 for now...)
-- 200 {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED
-- 201 {str session_key} {str ip}:{int2 port} LOGIN ACCEPTED - blah
--
--
-- Sample headers with known no data section, thus can parse next line:
-- 555 BANNED
-- {str reason}
--
-- 6xx INTERNAL SERVER ERROR 
-- ERROR: {str errormessage} 
--
-- 300 PONG 
-- {int4 port} (when nat=1) 
--
-- 998 VERSION 
-- {str server version} 
--
-- 208 UPTIME 
-- {int4 udpserver uptime in milliseconds} 
--
-- With imgServer enabled - Context/connection dependent
-- 200 {str session_key} LOGIN ACCEPTED 
-- {str image server name}
--
-- 201 {str session_key} LOGIN ACCEPTED - NEW VERSION AVAILABLE 
-- {str image server name} 
--
--
-- Probably unsupported headers:
-- 253 {int2 start} {int2 end} {int2 total} BUDDY LIST 
-- 254 {int2 start} {int2 end} {int2 total} BUDDY STATE
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
    return ([result] ++ dataz)

headers :: GenParser Char st [String]
headers = do
    comp <- compression
    aTag <- tag
    return_code <- returnCode
    char ' '
    return_string <- returnString
    eol
    return [show comp, aTag, show return_code, return_string]

-- For now just return true/false but i would like to "decompress"
-- the remaining data and then feed it back into the parser
compression :: GenParser Char st Bool
compression = 
    (string "00" >> return True)
    <|> (return False)

-- For now assume no limitation in length of tag
-- Need to tweak it so it can accept digits in the string
-- if i don't exclude digits it will eat up the return code if
-- the tag does not exist
tag :: GenParser Char st String
tag = do
        a <- many (noneOf " 0123456789")
        char ' '
        return a
    <|> (return "")

returnCode :: GenParser Char st Integer
returnCode = do 
    ds <- count 3 digit
    return $ read ds

returnString :: GenParser Char st String
returnString = many (noneOf "\n")

eol :: GenParser Char st Char
eol = char '\n'

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

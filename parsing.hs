-- Basic Anidb parser
import Text.ParserCombinators.Parsec


parseAnidb :: String -> Either ParseError [[String]]
parseAnidb input = parse anidbReply "(unknown)" input

-- Sample format
-- 
-- {three digit return code} {opt session key} {opt ip}:{opt port} {return string}\n
-- {three digit return code} {opt salt} {return string}\n
-- {opt compress (00)}{opt tag }{three digit return code} {str return string}{ - opt error message}\n
--
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
    return_code <- returnCode
    char ' '
    return_string <- returnString
    eol
    return [show return_code, return_string]

returnCode :: GenParser Char st Integer
returnCode = do 
    ds <- many1 digit
    return $ read ds

returnString :: GenParser Char st String
returnString = do
    many (noneOf "\n")

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
itemContent =
    many (noneOf "|\n")

module Anidb.Parse.Data
    ( parseCreator
    , AniDataParsed

    , parseCalendar
    , AniCalendarSeries
    , DateFlag

    , parseEpisode

    , AniData
    , ParseError

    , testCreator
    ) where

--import qualified Anidb.Parse.Reply as APR
import Control.Applicative hiding ((<|>), many)
import Data.Bits (testBit)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (readTime)
import Data.Word (Word8)
import System.Locale (defaultTimeLocale)
import Text.ParserCombinators.Parsec

-- Types
type AniDataParsed = Either ParseError AniData

data DateFlag = DateFlag {
      dfStartDay :: Bool --bit0 set == Startdate, Unknown Day
    , dfStartMonthDay :: Bool --bit1 set == Startdate, Unknown Month, Day
    , dfEndDay :: Bool --bit2 set == Enddate, Unknown Day
    , dfEndMonthDay :: Bool --bit3 set == Enddate, Unknown Month, Day
    , dfAiredOrEnded :: Bool --bit4 set == AirDate in the Past/Anime has ended
    , dfStartYear :: Bool --bit5 set == Startdate, Unknown Year
    , dfEndYear :: Bool --bit6 set == Enddate, Unknown Year
    } deriving (Show)

-- Aired/ToAir series
data AniCalendarSeries = AniCalendarSeries Integer UTCTime DateFlag
    deriving (Show)

-- Data parsed -- TODO: need to deal with "left over data"
data AniData =
    AniCreator {
          cCid :: Integer
        , cNameKanji :: String
        , cNameEnglish :: String
        , cType :: Integer
        , cPicture :: String
        , cUrlEnglish :: String
        , cUrlJapanese :: String
        , cWikiEnglish :: String
        , cWikiJapanese :: String
        , cLastUpdate :: UTCTime
        }
    | AniCalendar {
          caAired :: [AniCalendarSeries]
        , caToAir :: [AniCalendarSeries]
        }
    | AniEpisode {
          aeEid :: Integer
        , aeAid :: Integer
        , aeLength :: Integer
        , aeRating :: Integer -- TODO: really is a float i think
        , aeVotes :: Integer
        , aeEpNo :: String -- TODO: look into cleaning this up
        , aeNameEnglish :: String
        , aeNameRomaji :: String
        , aeNameKanji :: String
        , aeAired :: UTCTime
        }
    deriving (Show)


parseCreator :: String -> AniDataParsed
parseCreator input = parse anidbCreator "(unknown)" input

parseCalendar :: String -> AniDataParsed
parseCalendar input = parse anidbCalendar "(unknown)" input

parseEpisode :: String -> AniDataParsed
parseEpisode input = parse anidbEpisode "(unknown)" input

-- Anime Creator
anidbCreator :: GenParser Char st AniData
anidbCreator = AniCreator
        <$> parseInt -- {int creatorid}
        <*> parseStr -- {str creator name kanji}
        <*> parseStr -- {str creator name transcription}
        <*> parseInt -- TODO: {int type} - 1='person', 2='company', 3='collaboration'
        <*> parseStr -- {str pic_name}
        <*> parseStr -- {str url_english}
        <*> parseStr -- {str url_japanese}
        <*> parseStr -- {str wiki_url_english}
        <*> parseStr -- {str wiki_url_japanese}
        <*> parseDate -- {int last update date}
--        <*> eof

-- Anime Calendar
anidbCalendar :: GenParser Char st AniData
anidbCalendar = AniCalendar
    <$> count 25 parseAniCalendarSeries
    <*> many1 parseAniCalendarSeries
--    <*> eof

parseAniCalendarSeries :: GenParser Char st AniCalendarSeries
parseAniCalendarSeries = AniCalendarSeries
    <$> parseInt -- {int aid}
    <*> parseDate -- {int startdate}
    <*> parseDateFlag -- {int dateflags}

-- Anime Episode
anidbEpisode :: GenParser Char st AniData
anidbEpisode = AniEpisode
    <$> parseInt -- {int eid}
    <*> parseInt -- {int aid}
    <*> parseInt -- {int4 length}
    <*> parseInt -- {int4 rating}
    <*> parseInt -- {int votes}
    <*> parseEpNo -- {str epno}
    <*> parseStr -- {str eng}
    <*> parseStr -- {str romaji}
    <*> parseStr -- {str kanji}
    <*> parseDate -- {int aired}
--    <*> eof


-- TODO: deal with case where there's no integer to be parsed out
parseInt :: GenParser Char st Integer
parseInt = do
    int <- many1 digit
    eos
    return $ read int

parseStr :: GenParser Char st String
parseStr = do
    str <- many (noneOf "|\n")
    eos
    return str

-- TODO: deal with case where there's no timestamp to be parsed out
parseDate :: GenParser Char st UTCTime
parseDate = do
    time <- many1 digit
    eos
    return $ readTime defaultTimeLocale "%s" time

-- TODO: deal with breaking down a Integer to a sequence of word8
parseDateFlag :: GenParser Char st DateFlag
parseDateFlag = do
    word8Flag <- fromInteger <$> read <$> many1 digit :: GenParser Char st Word8
    eos
    return $ DateFlag
        (testBit word8Flag 0)
        (testBit word8Flag 1)
        (testBit word8Flag 2)
        (testBit word8Flag 3)
        (testBit word8Flag 4)
        (testBit word8Flag 5)
        (testBit word8Flag 6)

-- Special case parsing
-- Returned 'epno' includes special character (only if special) and padding (only if normal). Special characters are S(special), C(credits), T(trailer), P(parody), O(other).
parseEpNo :: GenParser Char st String
parseEpNo = do
    str <- many (noneOf "|\n")
    eos
    return str


eos :: GenParser Char st Char
eos = char '|' <|> char '\n'




-- Anidb reply parser test
testCreator :: String
testCreator = "718|GAINAX|Gainax|2|10092.png||http://www.gainax.co.jp/|Gainax|Gainax|1237048093\n"

testCalendar :: String
testCalendar = "1|1237048093|0\n2|1237048093|1\n3|1237048093|2\n9|1237048093|3\n4|1237048093|4\n10|1237048093|5\n15|1237048093|6\n31|1237048093|7\n5|1237048093|8\n11|1237048093|9\n16|1237048093|10\n32|1237048093|11\n20|1237048093|12\n33|1237048093|13\n34|1237048093|14\n35|1237048093|15\n6|1237048093|16\n12|1237048093|17\n17|1237048093|18\n36|1237048093|19\n21|1237048093|20\n37|1237048093|21\n38|1237048093|22\n39|1237048093|23\n24|1237048093|24\n40|1237048093|25\n41|1237048093|26\n42|1237048093|27\n43|1237048093|28\n44|1237048093|29\n45|1237048093|30\n"

testEpisode :: String
testEpisode = "2|1|24|750|2|02S|Kin of the Stars|Hoshi-tachi no Kenzoku|星たちの眷族|1295059229\n"


--animeDesc :: AN.AniNetState -> Integer -> IO APR.AniReplyParsed
--group :: AN.AniNetState -> GroupArg -> IO APR.AniReplyParsed
--
---- TODO: get confirmation on the syntax of the episode list
--character :: AN.AniNetState -> Integer -> IO APR.AniReplyParsed
---- TODO: get an actual sample of the syntax for the ep list
--groupStatus :: AN.AniNetState -> Integer -> Maybe Integer -> IO APR.AniReplyParsed
---- TODO: Need an idea of what the default anime/file mask is here, also
---- need a short anime mask here
--
--file :: AN.AniNetState -> FileArg -> FileMask -> FileAnimeMask -> IO APR.AniReplyParsed
---- TODO: do more in depth analysis of the mask
--anime :: AN.AniNetState -> AnimeArg -> Maybe AnimeMask -> IO APR.AniReplyParsed

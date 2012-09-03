module Anidb.Parse.Data
    ( parseCreator
    , AniDataParsed

    , parseCalendar
    , AniCalendarSeries
    , DateFlag

    , parseEpisode

    , parseAnimeDesc

    , parseGroup
    , GroupRelations
    , GroupRelationType

    , parseGroupStatus
    , CompletionState
    , GroupStatus

    , parseCharacter
    , AnimeBlocks
    , CharacterAppearance
    , CharacterType
    , CharacterGender

    , AniData
    , ParseError
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
      dfStartDay :: Bool --bit0 set == Startdate, Unknown Day -- Foundeddate
    , dfStartMonthDay :: Bool --bit1 set == Startdate, Unknown Month, Day -- foundeddate
    , dfEndDay :: Bool --bit2 set == Enddate, Unknown Day -- Disbandeddate
    , dfEndMonthDay :: Bool --bit3 set == Enddate, Unknown Month, Day -- Disbandeddate
    , dfAiredOrEnded :: Bool --bit4 set == AirDate in the Past/Anime has ended -- Ignored
    , dfStartYear :: Bool --bit5 set == Startdate, Unknown Year -- Foundeddate
    , dfEndYear :: Bool --bit6 set == Enddate, Unknown Year -- Disbandeddate
    } deriving (Show)

-- Aired/ToAir series
data AniCalendarSeries = AniCalendarSeries Integer UTCTime DateFlag
    deriving (Show)

-- Group relationship
data GroupRelations = GroupRelations Integer GroupRelationType
    deriving (Show)
data GroupRelationType = ParticipantIn | ParentOf | MergedFrom | NowKnownAs | Other | UnknownRelation
    deriving (Show)

-- Group Completion State
data CompletionState = Ongoing | Stalled | Complete | Dropped | Finished | SpecialsOnly | UnknownCompletion
    deriving (Show)

-- Group Status
data GroupStatus = GroupStatus {
      gsGid :: Integer
    , gsName :: String
    , gsCompletionState :: CompletionState
    , gsLastEpNo :: Integer
    , gsRating :: Integer
    , gsVotes :: Integer
    , gsEpisodeRange :: String -- TODO: find a better way to represent this
    } deriving (Show)

-- Anime block (relationship between character and anime series)
data AnimeBlocks = AnimeBlocks {
      abAid :: Integer
    , abAppearance :: CharacterAppearance
    , abCreatorId :: Maybe Integer
    , abMainSeiyuu :: Maybe Bool
    } deriving (Show)

-- Character appears
data CharacterAppearance = AppearsIn | CameoAppearanceIn | MainCharacterIn | SecondaryCastIn | UnknownAppearance
    deriving (Show)

-- Type of character
data CharacterType = Character | Mecha | Organisation | Vessel | UnknownType
    deriving (Show)

-- Gender of character
data CharacterGender = Male | Female | Intersexual | Dimorphic | NoneDoesNotApply | UnknownGender
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
    | AnimeDesc {
          adCurrentPart :: Integer
        , adMaxPart :: Integer
        , adDescription :: String
        }
    | AniGroup {
          agGid :: Integer
        , agRating :: Integer
        , agVotes :: Integer
        , agAccount :: Integer -- Unknown
        , agFileCount :: Integer
        , agName :: String
        , agShortName :: String
        , agIrcChannel :: String
        , agIrcServer :: String
        , agUrl :: String
        , agPicture :: String
        , agFoundedDate :: UTCTime
        , agDisbandedDate :: UTCTime
        , agDateFlag :: DateFlag
        , agLastReleaseDate :: UTCTime
        , agLastActivityDate :: UTCTime
        , agGroupRelations :: [GroupRelations]
        }
    | AniGroupStatus {
        agsGroupStatus :: [GroupStatus]
        }
    | AniCharacter {
          acCid :: Integer
        , acNameKanji :: String
        , acNameEnglish :: String
        , acPicture :: String
        , acAnimeBlocks :: [AnimeBlocks]
        , acEpisodeList :: String -- TODO "Unknown style" for episode list
        , acLastUpdate :: UTCTime
        , acType :: CharacterType
        , acGender :: CharacterGender
        }
    deriving (Show)


parseCreator :: String -> AniDataParsed
parseCreator input = parse anidbCreator "(unknown)" input

parseCalendar :: String -> AniDataParsed
parseCalendar input = parse anidbCalendar "(unknown)" input

parseEpisode :: String -> AniDataParsed
parseEpisode input = parse anidbEpisode "(unknown)" input

parseAnimeDesc :: String -> AniDataParsed
parseAnimeDesc input = parse anidbAnimeDesc "(unknown)" input

parseGroup :: String -> AniDataParsed
parseGroup input = parse anidbGroup "(unknown)" input

parseGroupStatus :: String -> AniDataParsed
parseGroupStatus input = parse anidbGroupStatus "(unknown)" input

parseCharacter :: String -> AniDataParsed
parseCharacter input = parse anidbCharacter "(unknown)" input

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
    <*> many parseAniCalendarSeries
--    <*> eof

parseAniCalendarSeries :: GenParser Char st AniCalendarSeries
parseAniCalendarSeries = AniCalendarSeries
    <$> parseInt -- {int aid}
    <*> parseDate -- {int startdate}
    <*> parseDateFlag -- {int dateflags}
--    <*> eol -- (\n)

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

-- Anime Description
anidbAnimeDesc :: GenParser Char st AniData
anidbAnimeDesc = AnimeDesc
    <$> parseInt -- {int4 current part}
    <*> parseInt -- {int4 max parts}
    <*> parseStr -- {str description}
--    <*> eof

-- Anime Group
anidbGroup :: GenParser Char st AniData
anidbGroup = AniGroup
    <$> parseInt -- {int gid}
    <*> parseInt -- {int4 rating}
    <*> parseInt -- {int votes}
    <*> parseInt -- {int4 acount}
    <*> parseInt -- {int fcount}
    <*> parseStr -- {str name}
    <*> parseStr -- {str short}
    <*> parseStr -- {str irc channel}
    <*> parseStr -- {str irc server}
    <*> parseStr -- {str url}
    <*> parseStr -- {str picname}
    <*> parseDate -- {int4 foundeddate}
    <*> parseDate -- {int4 disbandeddate}
    <*> parseDateFlag -- {int2 dateflags}
    <*> parseDate -- {int4 lastreleasedate}
    <*> parseDate -- {int4 lastactivitydate}
    <*> many parseGroupRelations -- {list grouprelations}
--    <*> eof

-- Group Relationship
parseGroupRelations :: GenParser Char st GroupRelations
parseGroupRelations = GroupRelations
    <$> parseListInt -- {int4 other group gid}
    <*> parseListGroupRelationType -- {int2 relationtype}
--    <*> eols -- ' | \n

-- Group relationship type
parseListGroupRelationType :: GenParser Char st GroupRelationType
parseListGroupRelationType = do
    relationType <- read <$> many1 digit :: GenParser Char st Integer
    eols
    return $ (case relationType of
        1 -> ParticipantIn
        2 -> ParentOf
        4 -> MergedFrom
        5 -> NowKnownAs
        6 -> Other
        _ -> UnknownRelation)

-- Anime Group Status
anidbGroupStatus :: GenParser Char st AniData
anidbGroupStatus = AniGroupStatus
    <$> many parseGroupStatusLine -- {list group status}
--    <*> eof

-- Group Status
parseGroupStatusLine :: GenParser Char st GroupStatus
parseGroupStatusLine = GroupStatus
    <$> parseInt -- {int group id}
    <*> parseStr -- {str group name}
    <*> parseCompletionState -- {int completion state}
    <*> parseInt -- {int last episode number}
    <*> parseInt -- {int rating}
    <*> parseInt -- {int votes}
    <*> parseEpisodeRange -- {str episode range}
--    <*> eol -- (\n)

parseCompletionState :: GenParser Char st CompletionState
parseCompletionState = do
    state <- read <$> many1 digit :: GenParser Char st Integer
    eos
    return $ (case state of
        1 -> Ongoing
        2 -> Stalled
        3 -> Complete
        4 -> Dropped
        5 -> Finished
        6 -> SpecialsOnly
        _ -> UnknownCompletion)

-- Anime Character
anidbCharacter :: GenParser Char st AniData
anidbCharacter = AniCharacter
    <$> parseInt -- {int charid}
    <*> parseStr -- {str character name kanji}
    <*> parseStr -- {str character name transcription}
    <*> parseStr -- {str pic}
    <*> many parseAnimeBlocks -- {anime blocks}
    <*> parseStr -- {int episode list} -- TODO: need actual sample format first
    <*> parseDate -- {int last update date}
    <*> parseCharacterType -- {int2 type}
    <*> parseCharacterGender -- {str gender}

parseAnimeBlocks :: GenParser Char st AnimeBlocks
parseAnimeBlocks = AnimeBlocks
    <$> parseListInt -- {int anime id}
    <*> parseListCharacterAppearance -- {int appearance}
    <*> parseMaybeListInt -- {int creatorid}
    <*> parseMaybeListBool -- {boolean is_main_seiyuu}

parseListCharacterAppearance :: GenParser Char st CharacterAppearance
parseListCharacterAppearance = do
    appears <- read <$> many1 digit :: GenParser Char st Integer
    try sls <|> eols
    return $ (case appears of
        0 -> AppearsIn
        1 -> CameoAppearanceIn
        2 -> MainCharacterIn
        3 -> SecondaryCastIn
        _ -> UnknownAppearance)

parseCharacterType :: GenParser Char st CharacterType
parseCharacterType = do
    cType <- read <$> many1 digit :: GenParser Char st Integer
    eos
    return $ (case cType of
        1 -> Character
        2 -> Mecha
        3 -> Organisation
        4 -> Vessel
        _ -> UnknownType)

parseCharacterGender :: GenParser Char st CharacterGender
parseCharacterGender = do
    gender <- many1 (noneOf "|\n")
    eos
    return $ (case gender of
        "M" -> Male
        "F" -> Female
        "I" -> Intersexual
        "D" -> Dimorphic
        "-" -> NoneDoesNotApply
        "?" -> UnknownGender
        _ -> UnknownGender)


parseMaybeListBool :: GenParser Char st (Maybe Bool)
parseMaybeListBool = do
    int <- optionMaybe (many1 digit)
    try sls <|> eols
    return $ parseBool int
    where
        parseBool Nothing  = Nothing
        parseBool (Just x) = (case (read x) of
            0 -> Just False
            1 -> Just True
            _ -> Nothing)

parseMaybeListInt :: GenParser Char st (Maybe Integer)
parseMaybeListInt = do
    int <- optionMaybe (many1 digit)
    try sls <|> eols
    return $ parseInt int
    where
        parseInt Nothing  = Nothing
        parseInt (Just x) = Just (read x)

-- TODO: deal with case where there's no integer to be parsed out
parseListInt :: GenParser Char st Integer
parseListInt = do
    int <- many1 digit
    try sls <|> eols
    return $ read int

-- TODO: deal with case where there's no integer to be parsed out
parseInt :: GenParser Char st Integer
parseInt = do
    int <- many1 digit
    eos
    return $ read int

-- TODO: deal with cleaning up the input string ie <br>, etc...
parseStr :: GenParser Char st String
parseStr = do
    str <- many (noneOf "|\n")
    eos
    return str

-- TODO: deal with case where there's no timestamp to be parsed out
-- TODO: deal with case where the timestamp is 0 - "unknown/not disbanded, etc"
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

-- TODO: Get an sample of the actual episode range, this is probably like "01,02-08,23-26"
parseEpisodeRange :: GenParser Char st String
parseEpisodeRange = do
    str <- many (noneOf "|\n")
    eos
    return str

-- End of section
eos :: GenParser Char st Char
eos = char '|' <|> char '\n'

-- End of list section
eols :: GenParser Char st Char
eols = char '\'' <|> char '|' <|> char '\n'

-- Next sub list section
sls :: GenParser Char st Char
sls = char ','


-- Anidb reply parser test
testCreator :: String
testCreator = "718|GAINAX|Gainax|2|10092.png||http://www.gainax.co.jp/|Gainax|Gainax|1237048093\n"

testCalendar :: String
testCalendar = "1|1237048093|0\n2|1237048093|1\n3|1237048093|2\n9|1237048093|3\n4|1237048093|4\n10|1237048093|5\n15|1237048093|6\n31|1237048093|7\n5|1237048093|8\n11|1237048093|9\n16|1237048093|10\n32|1237048093|11\n20|1237048093|12\n33|1237048093|13\n34|1237048093|14\n35|1237048093|15\n6|1237048093|16\n12|1237048093|17\n17|1237048093|18\n36|1237048093|19\n21|1237048093|20\n37|1237048093|21\n38|1237048093|22\n39|1237048093|23\n24|1237048093|24\n40|1237048093|25\n41|1237048093|26\n42|1237048093|27\n43|1237048093|28\n44|1237048093|29\n45|1237048093|30\n"

testEpisode :: String
testEpisode = "2|1|24|750|2|02S|Kin of the Stars|Hoshi-tachi no Kenzoku|星たちの眷族|1295059229\n"

testAnimeDesc :: String
testAnimeDesc = "0|1|As summer break arrives for the students, Jun Sakurada is busily studying on his own in the   library, making up for time lost <cut>\n"

testGroup :: String
testGroup = "7091|832|1445|43|566|Frostii|Frostii|#frostii|irc.rizon.net|http://frostii.com|15844.jpg|1228089600|0|1|1301875200|1304222640|7255,1'3097,4'748,4'8106,1'8159,2'8402,1'8696,1'9022,1\n"

testGroupStatus :: String
testGroupStatus = "123|Frostii|1|20|7231|2231|01-22\n121|Frost|5|19|0|0|02-22\n122|FrostY|2|11|7231|2231|09-22\n"

testCharacter :: String
testCharacter = "488|ニコ・ロビン|Nico Robin|14789.jpg|4097,2,1900,1'69,2,1901,0'6199,0,1900,1'5691,0,1900,1'2644,0,,'4851,0,1900,1||1236938094|1|F\n"


---- TODO: Need an idea of what the default anime/file mask is here, also
---- need a short anime mask here
--file :: AN.AniNetState -> FileArg -> FileMask -> FileAnimeMask -> IO APR.AniReplyParsed
---- TODO: do more in depth analysis of the mask
--anime :: AN.AniNetState -> AnimeArg -> Maybe AnimeMask -> IO APR.AniReplyParsed

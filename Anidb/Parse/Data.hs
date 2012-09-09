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

    , parseAnime
    , parseFile
    , ParseVal

    , Mask
    , MaskType(..)
    , hexMask
    , mkMask
    , mkMaskInt
    , valididateMask

    , AniData
    , ParseError
    ) where

--import qualified Anidb.Parse.Reply as APR
import Control.Applicative hiding ((<|>), many)
import Control.Monad (forM)
import Data.Bits (setBit, testBit, complement, (.&.))
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (readTime)
import Data.Word (Word8, Word64)
import Numeric (showHex)
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
    | AniCalendar [AniCalendarSeries] [AniCalendarSeries]
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
    | AniGroupStatus [GroupStatus]
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
    | AniAnime Mask [(Int, ParseVal)]
    | AniFiles [Integer]
    | AniFile Mask Mask Integer [(Int, ParseVal)] [(Int, ParseVal)] -- FileMask then ShortAnimeMask
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
    bool <- convertToBool <$> optionMaybe (many1 digit)
    try sls <|> eols
    return bool
    where
        convertToBool :: (Maybe String) -> (Maybe Bool)
        convertToBool Nothing  = Nothing
        convertToBool (Just x) = (case (read x) of
            0 -> Just False
            1 -> Just True
            _ -> Nothing)

parseBool :: GenParser Char st Bool
parseBool = do
    bool <- convertToBool <$> many1 digit
    eos
    return bool
    where
        convertToBool :: String -> Bool
        convertToBool "1" = True
        convertToBool _   = False

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

-- TODO: deal with cleaning up the input string ie <br>, etc...
parseListOfInt :: GenParser Char st [Integer]
parseListOfInt = do
    intl <- sepEndBy (read <$> many1 digit) sls
    eos
    return intl

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

-- TODO: deal with cleaning up the input string ie <br>, etc...
parseListOfStr :: GenParser Char st [String]
parseListOfStr = do
    strl <- sepEndBy (many1 (noneOf "\',|\n")) ssls
    eos
    return strl

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

ssls :: GenParser Char st Char
ssls = char ',' <|> char '\''


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

testAnime :: String
testAnime = "1|1999-1999|TV Series|Space,Future,Plot Continuity,SciFi,Space Travel,Shipboard,Other Planet,Novel,Genetic Modification,Action,Romance,Military,Large Breasts,Gunfights,Adventure,Human Enhancement,Nudity|Seikai no Monshou|星界の紋章|Crest of the Stars|Testing'Star|13|13|3|853|3225|756|110|875|11\n"

testAnimeMask :: String
testAnimeMask = "b2f0e0fc000000"

testFile :: String
testFile = "312498|4688|69260|4243|0||0|1|177747474|70cd93fd3981cc80a8ea6a646ff805c9|b2a7c7d591333e20495de3571b235c28|7af9b962c17ff729baeee67533e5219526cd5095|a200fe73|high|DTV|Vorbis (Ogg Vorbis)|104|H264/AVC|800|704x400|japanese|english'english'english|1560||1175472000|26|26|01|The Wings to the Sky|Sora he no Tsubasa|星界の紋章|Nanoha-DGz\n"

testFiles :: String
testFiles = "312498|4688|69260|4243\n"

testFileMask :: String
testFileMask = "7ff8fef800"

testShortAnimeMask :: String
testShortAnimeMask = "d0203080"


-- For now have some sort of dummy wrapper that will extract the relevant flag/value out
-- of the data, but latter i would like to have a records alike syntax or lens like
-- syntax, perhaps can even just do an automated thin lens wrapper with some sort of
-- value lookup.
--
-- Would need to define a new ADT - one value for each flag and then use that in the
-- lookup, but at least its not going to be a 50+ parameter record construction....
--
-- I think the ADT -> flag bit and value is the best approach so you can like generate
-- a new flag via yielding up a list of ADT for the relevant data that you are interested
-- in
parseAnime :: Mask -> String -> AniDataParsed
parseAnime mask input = parse (anidbAnime mask) "(unknown)" input

parseFile :: Mask -> Mask -> Bool -> String -> AniDataParsed
parseFile fmask samask multiples input = if multiples
    then parse anidbFiles "(unknown)" input
    else parse (anidbFile fmask samask) "(unknown)" input

-- This is a bit of a tricky one, but basically
-- It takes a list of ParseType and their index from idxList which
-- operates on the Mask and then execute a parser for that ParseType
-- and store it all in a list which is returned [(Idx, ParseVal)]
anidbAnime :: Mask -> GenParser Char st AniData
anidbAnime mask = AniAnime mask
    <$> forM (idxList mask) (\(n,t) -> parsingValue t >>= (\v -> return (n, v)))

-- TODO: Improve type safty of this, we have 3 types of masks, should disassocate it
anidbFile :: Mask -> Mask -> GenParser Char st AniData
anidbFile fmask samask = AniFile fmask samask
    <$> parseInt -- {int4 fid}
    <*> forM (idxList fmask) (\(n,t) -> parsingValue t >>= (\v -> return (n, v)))
    <*> forM (idxList samask) (\(n,t) -> parsingValue t >>= (\v -> return (n, v)))

anidbFiles :: GenParser Char st AniData
anidbFiles = AniFiles
    <$> many1 parseInt -- {int4 fid}

-- This maps from a ParseType to the ParseValue
parsingValue :: ParseType -> GenParser Char st ParseVal
parsingValue PTBool      = PVBool <$> parseBool
parsingValue PTDate      = PVDate <$> parseDate
parsingValue PTDateFlag  = PVDateFlag <$> parseDateFlag
parsingValue PTFileState = PVFileState <$> parseStr -- TODO: convert to FileState type
parsingValue PTInt       = PVInt <$> parseInt
parsingValue PTIntList   = PVIntList <$> parseListOfInt
parsingValue PTStr       = PVStr <$> parseStr
parsingValue PTStrList   = PVStrList <$> parseListOfStr

data ParseVal  = PVInt Integer -- {plain integer}
               | PVIntList [Integer] -- {integer list} - ,
               | PVStr String -- {plain string}
               | PVStrList [String] -- {string list} - ',
               | PVBool Bool -- {boolean} -- TODO: clean this up
               | PVDate UTCTime -- {UTCTime}
               | PVDateFlag DateFlag -- {DateFlag}
               | PVFileState String -- {FileState} -- TODO: convert to FileState
    deriving (Show)

data ParseType = PTInt -- {plain integer}
               | PTIntList -- {integer list} - ,
               | PTStr -- {plain string}
               | PTStrList -- {string list} - ',
               | PTBool -- {boolean}
               | PTDate -- {UTCTime}
               | PTDateFlag -- {DateFlag}
               | PTFileState -- {FileState}
    deriving (Show)

-- TODO: May be able to abstract away MaskType via TypeClass
data MaskType = ShortAnimeMask | LongAnimeMask | FileMask
    deriving (Show)

data Mask = Mask MaskType Word64
    deriving (Show)

hexMask :: Mask -> String
hexMask (Mask _ x) = showHex x ""

mkMask :: MaskType -> String -> Mask
mkMask a hexstr = Mask a (mkWord64 hexstr)
    where
        mkWord64 :: String -> Word64
        mkWord64 s@('0':'x':_) = fromIntegral (read s) :: Word64
        mkWord64 s@('0':'X':_) = fromIntegral (read s) :: Word64
        mkWord64 s             = fromIntegral (read ("0x" ++ s)) :: Word64

mkMaskInt :: MaskType -> [Int] -> Mask
mkMaskInt a i = Mask a (mkWord64 i)
    where
        mkWord64 :: [Int] -> Word64
        mkWord64 = foldl (\a x -> setBit a x) 0

-- Return True if the mask is known good, False otherwise
valididateMask :: Mask -> Bool
valididateMask (Mask m x) = (x .&. complement (idx2Mask $ validMaskField m)) == 0
    where
        idx2Mask :: [(Int, Int, ParseType)] -> Word64
        idx2Mask = foldl (\a (x, y, _) -> setBit a (x*8+y)) 0

-- Generate a list of idx pointers from a mask
idxList :: Mask -> [(Int, ParseType)]
idxList (Mask m x) = sortBy myPredicate $ catMaybes $ map (isSet x) (validMaskField m)
    where
        isSet :: Word64 -> (Int, Int, ParseType) -> Maybe (Int, ParseType)
        isSet a (x, y, t)
            | testBit a (x*8+y) = Just ((x*8+y), t)
            | otherwise         = Nothing

        myPredicate :: (Int, b) -> (Int, b) -> Ordering
        myPredicate (a1, _) (b1, _) = compare b1 a1

-- Explaination, its (Int, Int), in which it is
-- Byte x, Bit y - from lsb (rightmost byte 0, bit 0)
-- to msb (leftmost byte 4,5,7 bit 7)
-- Undefined = not listed
validMaskField :: MaskType -> [(Int, Int, ParseType)]
validMaskField ShortAnimeMask =
    [ (0, 0, PTDate) -- {int4 date aid record updated}
    , (0, 6, PTStr) -- {str group short name}
    , (0, 7, PTStr) -- {str group name}
    , (1, 2, PTInt) -- {int4 episode vote count}
    , (1, 3, PTInt) -- {int4 episode rating}
    , (1, 4, PTStr) -- {str ep kanji name}
    , (1, 5, PTStr) -- {str ep romaji name}
    , (1, 6, PTStr) -- {str ep name}
    , (1, 7, PTStr) -- {str epno}
    , (2, 2, PTStrList) -- {str synonym list}
    , (2, 3, PTStrList) -- {str short name list}
    , (2, 4, PTStrList) -- {str other name}
    , (2, 5, PTStr) -- {str english name}
    , (2, 6, PTStr) -- {str kanji name}
    , (2, 7, PTStr) -- {str romaji name}
    , (3, 1, PTStrList) -- {str category list}
    , (3, 2, PTStr) -- {str related aid type}
    , (3, 3, PTStrList) -- {str related aid list}
    , (3, 4, PTStr) -- {str type}
    , (3, 5, PTStr) -- {str year}
    , (3, 6, PTInt) -- {int4 highest episode number}
    , (3, 7, PTInt) -- {int4 anime total episodes}
    ]
validMaskField FileMask =
    [ (0, 1, PTStr) -- {str mylist other}
    , (0, 2, PTStr) -- {str mylist source}
    , (0, 3, PTStr) -- {str mylist storage}
    , (0, 4, PTDate) -- {int4 mylist viewdate}
    , (0, 5, PTInt) -- {int4 mylist viewed}
    , (0, 6, PTInt) -- {int4 mylist filestate}
    , (0, 7, PTInt) -- {int4 mylist state}
    , (1, 0, PTStr) -- {str anidb file name}
    , (1, 3, PTDate) -- {int4 aired date}
    , (1, 4, PTStr) -- {str description}
    , (1, 5, PTInt) -- {int4 length in seconds}
    , (1, 6, PTStrList) -- {str sub language}
    , (1, 7, PTStr) -- {str dub language}
    , (2, 0, PTStr) -- {str file type (extension)}
    , (2, 1, PTStr) -- {str video resolution}
    , (2, 2, PTInt) -- {int4 video bitrate}
    , (2, 3, PTStr) -- {str video codec}
    , (2, 4, PTIntList) -- {int4 audio bitrate list}
    , (2, 5, PTStrList) -- {str audio codec list}
    , (2, 6, PTStr) -- {str source}
    , (2, 7, PTStr) -- {str quality}
    , (3, 1, PTStr) -- {video colour depth} - ? (str for now)
    , (3, 3, PTStr) -- {str crc32}
    , (3, 4, PTStr) -- {str sha1}
    , (3, 5, PTStr) -- {str md5}
    , (3, 6, PTStr) -- {str ed2k}
    , (3, 7, PTInt) -- {int8 size}
    , (4, 0, PTFileState) -- {int2 state} - State Type
    , (4, 1, PTInt) -- {int2 IsDeprecated}
    , (4, 2, PTStrList) -- {list other episodes} - ? (str for now)
    , (4, 3, PTInt) -- {int4 mylist id}
    , (4, 4, PTInt) -- {int4 gid}
    , (4, 5, PTInt) -- {int4 eid}
    , (4, 6, PTInt) -- {int4 aid}
    ]
validMaskField LongAnimeMask =
    [ (0, 3, PTInt) -- {int4 parody count}
    , (0, 4, PTInt) -- {int4 trailer count}
    , (0, 5, PTInt) -- {int4 other count}
    , (0, 6, PTInt) -- {int4 credits count}
    , (0, 7, PTInt) -- {int4 specials count}
    , (1, 4, PTStrList) -- {str main creator name list}
    , (1, 5, PTIntList) -- {int main creator id list}
    , (1, 6, PTIntList) -- {int creator id list}
    , (1, 7, PTIntList) -- {int character id list}
    , (2, 0, PTDate) -- {int date record updated}
    , (2, 4, PTStr) -- {str AnimeNfo id}
    , (2, 5, PTInt) -- {int allcinema id}
    , (2, 6, PTInt) -- {int ANN id}
    , (2, 7, PTInt) -- {int anime planet id}
    , (3, 0, PTBool) -- {bool is 18+ restricted}
    , (3, 1, PTStrList) -- {str award list}
    , (3, 2, PTInt) -- {int review count}
    , (3, 3, PTInt) -- {int4 average review rating}
    , (3, 4, PTInt) -- {int temp vote count}
    , (3, 5, PTInt) -- {int4 temp rating}
    , (3, 6, PTInt) -- {int vote count}
    , (3, 7, PTInt) -- {int4 rating}
    , (4, 0, PTStrList) -- {str category id list}
    , (4, 1, PTStr) -- {str picname}
    , (4, 2, PTStr) -- {str url}
    , (4, 3, PTDate) -- {int end date}
    , (4, 4, PTDate) -- {int air date}
    , (4, 5, PTInt) -- {int4 special ep count}
    , (4, 6, PTInt) -- {int4 highest episode number}
    , (4, 7, PTInt) -- {int4 episodes}
    , (5, 2, PTStrList) -- {str synonym list}
    , (5, 3, PTStrList) -- {str short name list}
    , (5, 4, PTStrList) -- {str other name}
    , (5, 5, PTStr) -- {str english name}
    , (5, 6, PTStr) -- {str kanji name}
    , (5, 7, PTStr) -- {str romaji name}
    , (6, 0, PTStrList) -- {str category weight list}
    , (6, 1, PTStrList) -- {str category list}
    , (6, 2, PTStr) -- {str related aid type} - (?)Type Related
    , (6, 3, PTStrList) -- {str related aid list}
    , (6, 4, PTStr) -- {str type}
    , (6, 5, PTStr) -- {str year}
    , (6, 6, PTDateFlag) -- {int dateflags}
    , (6, 7, PTInt) -- {int aid}
    ]

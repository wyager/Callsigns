{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Set (Set, notMember, member)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)

-- A 5-digit callsign
data CS = CS !Char !Char !Char !Char !Char deriving (Eq,Ord,Show)

-- Callsign expires or callsign is created/renewed/etc.
data Event = Exp | Other deriving Show

-- An event and its associated callsign
data Entry = Entry CS Event deriving Show

-- Take a list of database entries and build a set of in-use 5-digit callsigns
processDB :: [Entry] -> Set CS
processDB = foldl' processDB' Set.empty 

-- Process a single database entry and update the set
processDB' :: Set CS -> Entry -> Set CS
processDB' !things (Entry cs event) = case event of
    Exp -> Set.delete cs things
    Other -> Set.insert cs things

-- Read a line from the DB
parseEntry :: Text -> Maybe Entry
parseEntry text
    | Just cs' <- parseCS cs
    = Just (Entry cs' event') 
        where 
        [_,_,_,cs,_,event] = T.splitOn "|" text
        event' = if event == "LIEXP" then Exp else Other
parseEntry _ = Nothing

-- Parse a 5-digit callsign
parseCS :: Text -> Maybe CS
parseCS text
    | T.length text == 5 
    = Just $ CS (at 0) (at 1) (at 2) (at 3) (at 4)
        where at n = T.index text n
parseCS _ = Nothing

-- "john" becomes
-- ["ohn", "jhn", "jon", "jho"]
strip :: Text -> [Text]
strip t = case T.uncons t of
    Just (c,cs) -> cs : (T.cons c <$> strip cs)
    Nothing -> []

-- Pluralize a word. "cat" becomes "cats".
s :: Text -> Text
s t = T.snoc t 's'

-- Given a word, generate possible callsign-sized words from it.
expand :: Text -> [Text]
expand text = case T.length text of
    4 -> [s text]
    5 -> [text]
    6 -> strip text
    _ -> []

-- The letters that can be represented by a number. E.g. "E" looks like "3".
letters = "aAbBeEiIlLoOsStTgG"
numbers = "448833111100557766"

numSet = Set.fromList letters
numMap = Map.fromList (zip letters numbers)

-- Is a letter like a number?
num x = x `member` numSet

-- What's the number it looks like?
number x = case Map.lookup x numMap of
    Just n -> n
    Nothing -> error ("Invalid number-like character: " ++ [x])

-- Check if a string can be turned into a callsign
good :: Text -> Bool
good text = case parseCS text of
    Just (CS a b c d e) -> start && num b && letter c && letter d && letter e
        where
        start = a == 'k' || a == 'w' || a == 'n'
             || a == 'K' || a == 'W' || a == 'N'
        letter x = ('A' <= x && x <= 'Z')
                || ('a' <= x && x <= 'z')
    _ -> False

-- Check if a word is unused
unused :: Set CS -> Text -> Bool
unused db word = case parseCS (T.toUpper word) of
        Just (CS a b c d e) -> CS a (number b) c d e `notMember` db

-- Get all possible words that make sense as callsigns and aren't used
process :: Set CS -> [Text] -> [Text]
process db words = [word | initial <- words, word <- expand initial, good word, unused db word]

-- Load the database from "./l_amat/HS.dat"
db :: IO (Set CS)
db = processDB . catMaybes . map parseEntry . T.lines <$> TIO.readFile "HS.dat"

-- Cat a dictionary file into this program to generate words from
main = do
    db <- db
    words <- T.lines <$> TIO.getContents
    let nub = Set.toList . Set.fromList
    mapM_ print $ nub $ process db words

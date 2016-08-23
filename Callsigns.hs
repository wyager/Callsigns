{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Data.List (foldl') -- To reduce a list
import qualified Data.Map.Strict as Map
import Data.Set (Set, notMember)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (catMaybes)
import Data.Char (isDigit, isUpper, toUpper)
import Control.Monad (guard) -- To check the validity of a callsign
import Control.Applicative ((<$>)) -- Inline version of fmap

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

-- Parse a 5-digit callsign. Assume it's formatted correctly.
parseCS :: Text -> Maybe CS
parseCS text | T.length text == 5 
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
letters = "ABEILOSTG"
numbers = "483110576"
numMap  = Map.fromList (zip letters numbers)

-- Generate a callsign from some text.
-- E.g. "nergy" becomes "N3RGY" (yours truly).
-- If the text can't be spelled as a 5-digit callsign, return Nothing.
toCS :: Text -> Maybe CS
toCS text = do
    guard (T.length text == 5)
    let [a,b,c,d,e] = [toUpper (T.index text n) | n <- [0..4]]
    number <- Map.lookup b numMap
    let cs = CS a number c d e
    guard (valid cs)
    return cs

-- Check if a callsign is valid
valid :: CS -> Bool
valid (CS a b c d e) = startIsValid && isDigit b && isUpper c && isUpper d && isUpper e
    where startIsValid = a == 'K' || a == 'W' || a == 'N'

-- Get all possible words that make sense as callsigns and aren't used
process :: Set CS -> [Text] -> [Text]
process used words = do
    initial <- words
    word <- expand initial
    case toCS word of
        Just cs | cs `notMember` used -> return word
        _                             -> fail "Word can't be turned into a new callsign"

-- Load the database
loadDB :: FilePath -> IO (Set CS)
loadDB path = processDB . catMaybes . map parseEntry . T.lines <$> TIO.readFile path

-- Cat a dictionary file into this program to generate words from
main = do
    db <- loadDB "HS.dat"
    words <- T.lines <$> TIO.getContents
    let nub = Set.toList . Set.fromList -- Remove duplicates
    mapM_ print $ nub $ process db words

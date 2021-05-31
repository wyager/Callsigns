{-# LANGUAGE BangPatterns, OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections #-}
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Char (toUpper)
import           Data.Attoparsec.ByteString.Char8 (Parser, string, takeTill, char, isEndOfLine, endOfLine)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Streaming.ByteString as SB
import qualified Streaming.ByteString.Char8 as SB8
import qualified Data.Attoparsec.ByteString.Streaming as ABS
import qualified Streaming.Prelude as SP
import           Streaming.Prelude (Of((:>)))
import           Control.Monad (guard, unless)
import           Control.Applicative ((<$>), (<|>)) 
import           System.IO (openFile, IOMode(ReadMode))
import           Control.Monad.Trans.Resource (runResourceT)

newtype Callsign = Callsign {textOf :: ByteString} deriving (Show, Eq, Ord, Hashable)

data Event = Expired | Other  deriving (Show, Eq, Ord)

callsignEvent :: Parser (Callsign, Event)
callsignEvent = do
    let column = takeTill (== '|') <* char '|'
    string "HS|"
    _id <- column
    _ <- column
    callsign <- Callsign <$> column
    _date <- column
    event <-  (Expired <$ string "LIEXP ") <|> 
              (Other <$ takeTill (=='\n'))
    endOfLine
    return (callsign, event)

callsignWords :: IO (HashMap.HashMap Callsign ByteString)
callsignWords = do
    let dict = SB.readFile "/usr/share/dict/web2"
    let words = ABS.parsed (takeTill (=='\n') <* endOfLine) dict
    let callsignsOfWords =
            SP.for words $ \word ->
                SP.for (SP.each $ expand word) $ \expanded ->
                    SP.each $ (,expanded) <$> callsignFor expanded
    let insert map (cs,txt) = HashMap.insert cs txt map
    let loadCallsigns = SP.fold insert HashMap.empty id callsignsOfWords
    callsignToWord :> result <- runResourceT loadCallsigns
    () <- either (error . show . fst) return result
    return callsignToWord

alreadyUsed :: (Callsign -> Bool) -> IO (HashSet.HashSet Callsign)
alreadyUsed isInteresting = do
    let hs = SB.readFile "HS.dat" 
    let events = ABS.parsed callsignEvent hs
    let interestingEvents = SP.filter (isInteresting . fst) events
    let include set (callsign,event) = if event == Expired 
        then HashSet.delete callsign set
        else HashSet.insert callsign set
    let loadUsed = SP.fold include HashSet.empty id interestingEvents
    used :> result <- runResourceT loadUsed 
    () <- either (error . show . fst) return result
    return used

main :: IO ()
main = do
    goodWords <- callsignWords
    alreadyUsed <- alreadyUsed (`HashMap.member` goodWords)
    flip mapM_ (HashMap.toList goodWords) $ \(callsign,word) -> do
        unless (callsign `HashSet.member` alreadyUsed) $ do
            BS8.putStrLn $ word <> " ~> " <> textOf callsign

-- Given a word, generate possible callsign-sized words from it.
expand :: ByteString -> [ByteString]
expand text = case BS.length text of
    4 -> [text `BS8.snoc` 's']
    5 -> [text]
    6 -> zipWith (<>) (BS.inits text) (tail (BS.tails text))
    _ -> []

-- The letters that can be represented by a number. E.g. "E" looks like "3".
numberFor :: Char -> Maybe Char
numberFor c = case c of
    'A' -> Just '4'; 'B' -> Just '8'; 'E' -> Just '3'; 'I' -> Just '1'; 'L' -> Just '1';
    'O' -> Just '0'; 'S' -> Just '5'; 'T' -> Just '7'; 'G' -> Just '6'; _   -> Nothing

-- -- Generate a callsign from some text.
-- -- E.g. "nergy" becomes "N3RGY" (yours truly).
-- -- If the text can't be spelled as a 5-digit callsign, return Nothing.
callsignFor :: ByteString -> Maybe Callsign
callsignFor text = do
    guard $ (BS.length text == 5) && (a == 'K' || a == 'W' || a == 'N')
    b' <- numberFor b
    return $ Callsign (a `BS8.cons` b' `BS8.cons` BS.drop 2 text')
    where
    text' = BS8.map toUpper text
    at = BS8.index text'
    a = at 0
    b = at 1
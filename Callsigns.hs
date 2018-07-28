{-# LANGUAGE BangPatterns, OverloadedStrings, GeneralizedNewtypeDeriving, TupleSections #-}
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (toUpper)
import           Data.Attoparsec.Text (Parser, string, char, takeTill, isEndOfLine, endOfLine)
import qualified Pipes.Attoparsec as PA
import           Pipes ((>->))
import           Pipes.Text.IO (fromHandle)
import qualified Pipes.Prelude as P
import           Control.Monad (guard, unless)
import           Control.Applicative ((<$>), (<|>)) 
import           System.IO (openFile, IOMode(ReadMode))

newtype Callsign = Callsign {textOf :: T.Text} deriving (Show, Eq, Ord, Hashable)

data Event = Expired | Other  deriving (Show, Eq, Ord)

callsignEvent :: Parser (Callsign, Event)
callsignEvent = do
    let column = takeTill (== '|') <* char '|'
    string "HS|"
    _id <- column
    _ <- column
    callsign <- Callsign <$> column
    _date <- column
    event <-  (string "LIEXP "      *> return Expired) <|> 
              (takeTill isEndOfLine *> return Other)
    endOfLine
    return (callsign, event)

callsignWords :: IO (HashMap.HashMap Callsign Text)
callsignWords = do
    dict <- openFile "/usr/share/dict/web2" ReadMode
    let words = PA.parsed (takeTill isEndOfLine <* endOfLine) (fromHandle dict)
    let callsignsOfWords = words 
                        >-> P.mapFoldable expand 
                        >-> P.mapFoldable (\text -> (,text) <$> callsignFor text)
    let insert map (cs,txt) = HashMap.insert cs txt map
    let mapOf = P.fold' insert HashMap.empty id
    (callsignToWord, result) <- mapOf callsignsOfWords
    _ <- either (error . show . fst) return result
    return callsignToWord

alreadyUsed :: (Callsign -> Bool) -> IO (HashSet.HashSet Callsign)
alreadyUsed isInteresting = do
    hs <- openFile "HS.dat" ReadMode
    let events = PA.parsed callsignEvent (fromHandle hs)
    let interestingEvents = events >-> P.filter (isInteresting . fst)
    let include set (callsign,event) = if event == Expired 
        then HashSet.delete callsign set
        else HashSet.insert callsign set
    let latest = P.fold' include HashSet.empty id
    (used,result) <- latest interestingEvents
    _ <- either (error . show . fst) return result
    return used

main :: IO ()
main = do
    goodWords <- callsignWords
    alreadyUsed <- used (`HashMap.member` goodWords)
    flip mapM_ (HashMap.toList goodWords) $ \(callsign,word) -> do
        unless (callsign `HashSet.member` alreadyUsed) $ do
            TIO.putStrLn $ word `T.append` " ~> " `T.append` textOf callsign

-- Given a word, generate possible callsign-sized words from it.
expand :: Text -> [Text]
expand text = case T.length text of
    4 -> [text `T.snoc` 's']
    5 -> [text]
    6 -> zipWith T.append (T.inits text) (tail (T.tails text))
    _ -> []

-- The letters that can be represented by a number. E.g. "E" looks like "3".
numberFor :: Char -> Maybe Char
numberFor c = case c of
    'A' -> Just '4'; 'B' -> Just '8'; 'E' -> Just '3'; 'I' -> Just '1'; 'L' -> Just '1';
    'O' -> Just '0'; 'S' -> Just '5'; 'T' -> Just '7'; 'G' -> Just '6'; _   -> Nothing

-- -- Generate a callsign from some text.
-- -- E.g. "nergy" becomes "N3RGY" (yours truly).
-- -- If the text can't be spelled as a 5-digit callsign, return Nothing.
callsignFor :: Text -> Maybe Callsign
callsignFor text = do
    guard $ (T.length text == 5) && (a == 'K' || a == 'W' || a == 'N')
    b' <- numberFor b
    return $ Callsign (a `T.cons` b' `T.cons` T.drop 2 text')
    where
    text' = T.map toUpper text
    at = T.index text'
    a = at 0
    b = at 1
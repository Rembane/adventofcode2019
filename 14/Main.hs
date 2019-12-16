import           Control.Monad.Combinators                ( sepBy1 )
import           Data.Maybe
import qualified Data.Map.Strict               as M
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Debug.Trace

type Parser = Parsec Void String
data Chemical = Chemical Int String
  deriving Show

chemicalParser :: Parser Chemical
chemicalParser =
  Chemical <$> (fmap read (some digitChar) <* space) <*> some upperChar

reactionParser :: Parser (String, (Int, [Chemical]))
reactionParser =
  (\cs (Chemical i c) -> (c, (i, cs)))
    <$> sepBy1 chemicalParser (string ", ")
    <*> (string " => " *> chemicalParser <* optional eol)

part1
  :: M.Map String (Int, [Chemical])
  -> M.Map String Int
  -> String
  -> Int
  -> M.Map String Int
part1 _        l      "ORE"  _        = l
part1 recipies larder needle quantity = case M.lookup needle recipies of
  Nothing      -> error $ "I could not find " <> needle
  Just (n, cs) -> trace (show (needle, quantity, larder)) $ M.insertWith
    (+)
    needle
    quantity
    (foldr (\(Chemical j s) l' -> part1 recipies l' s j) larder cs)

main :: IO ()
main = do
  chemicals <- parse (some reactionParser) "Problem input"
    <$> readFile "input3.txt"
  case chemicals of
    Left  e -> error (show e)
    Right r -> do
      let ores = concatMap
            (\(b, (i, cs)) -> mapMaybe
              (\(Chemical k n) -> if n == "ORE" then Just (i, k, b) else Nothing
              )
              cs
            )
            r
      let m = part1 (traceShowId $ M.fromList r) M.empty "FUEL" 1
      print ores -- 3$ map (\(i,k,n) -> ) ores
      print m


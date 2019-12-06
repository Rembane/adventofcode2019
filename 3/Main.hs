import           Control.Arrow
import           Control.Monad.Combinators
import           Data.Function
import           Data.List
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Void
import           Text.Megaparsec                          ( Parsec
                                                          , parse
                                                          )
import           Text.Megaparsec.Char                     ( char
                                                          , digitChar
                                                          , newline
                                                          )

type Parser = Parsec Void String

-- | (x,y)
-- v -->
newtype Pos = Pos { unPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

withPos :: ((Int, Int) -> (Int, Int)) -> Pos -> Pos
withPos f = Pos . f . unPos

vParser :: Parser (Pos -> Pos)
vParser = do
  c <- choice (map char "UDRL")
  d <- read <$> some digitChar
  pure $ withPos $ case c of
    'U' -> second (subtract d)
    'D' -> second (+ d)
    'R' -> first (+ d)
    'L' -> first (subtract d)

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

trace :: (Pos, Pos) -> [Pos]
trace =
  map Pos
    . tail
    . uncurry zip
    . both (\(i1, i2) -> inf $ enumFromThenTo i1 (next i1 i2) i2)
    . tt
    . both unPos
 where
  inf [v] = repeat v
  inf vs  = vs
  next i1 i2 = case compare i1 i2 of
    LT -> i1 + 1
    EQ -> i1
    GT -> i1 - 1

allIntersections :: [S.Set Pos] -> S.Set Pos
allIntersections = S.delete (Pos (0, 0)) . foldr1 S.intersection

part1 :: S.Set Pos -> Int
part1 = S.findMin . S.map (uncurry (+) . both abs . unPos)

part2 :: S.Set Pos -> [[Pos]] -> Int
part2 is = minimum . foldr1 (M.unionWith (+)) . map
  ( M.fromListWith const
  . counter 1
  . concat
  . uncurry (zipWith (curry trace))
  . (id &&& tail)
  )
 where
  counter _ [] = []
  counter i (p : ps) | S.member p is = (p, i) : counter (i + 1) ps
                     | otherwise     = counter (i + 1) ps

main :: IO ()
main =
  readFile "input.txt"
    >>= (parse (some ((vParser `sepBy1` char ',') <* optional newline)) "Glrgh"
        >>> either
              print
              (   map (scanl' (&) (Pos (0, 0)))
              >>> (\ps ->
                    let
                      is =
                        ( allIntersections
                          . map
                              ( S.fromList
                              . concat
                              . uncurry (zipWith (curry trace))
                              . (id &&& tail)
                              )
                          )
                          ps
                    in  print (part1 is) >> print (part2 is ps)
                  )
              )
        )

{-# LANGUAGE LambdaCase #-}
import           Control.Arrow
import           Control.Monad.Combinators
import           Data.Function
import           Data.Ix
import           Data.List
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
    . uncurry zip
    . both (inf . range . (uncurry min &&& uncurry max))
    . tt
    . both unPos
 where
  inf [v] = repeat v
  inf vs  = vs


main :: IO ()
main =
  readFile "input.txt"
    >>= (parse (some ((vParser `sepBy1` char ',') <* optional newline)) "Glrgh"
        >>> \case
              Left e -> print e
              Right fs ->
                print
                  $ S.findMin
                  $ S.map (uncurry (+) . both abs . unPos)
                  $ S.delete (Pos (0, 0))
                  $ foldr1 S.intersection
                  $ map
                      ( S.fromList
                      . concat
                      . uncurry (zipWith (curry trace))
                      . (id &&& tail)
                      . scanl' (&) (Pos (0, 0))
                      )
                      fs
        )

{-# LANGUAGE TupleSections #-}
import           Control.Applicative                      ( liftA2 )
import           Control.Arrow                            ( (&&&)
                                                          , (***)
                                                          , (>>>)
                                                          , first
                                                          , second
                                                          )
import           Data.Char                                ( isUpper )
import           Data.Foldable                            ( find )
import           Data.Function                            ( on )
import           Data.List                                ( foldl'
                                                          , groupBy
                                                          , partition
                                                          , sort
                                                          )
import           Data.Maybe                               ( fromJust
                                                          , mapMaybe
                                                          )
import qualified Data.Map.Strict               as M
import qualified Data.Sequence                 as Sq
import           Data.Tuple                               ( swap )

import           Debug.Trace

newtype Pos = Pos { unPos :: (Int, Int) }
  deriving (Eq, Ord, Show)

data Cell = Empty | Portal String
  deriving (Eq, Ord, Show)

-- | ...as the rook goes
allSurrounding :: Pos -> [Pos]
allSurrounding p = map (\f -> Pos $ f $ unPos p)
                       (($) <$> [first, second] <*> [subtract 1, (+ 1)])

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

dot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dot = (.) . (.)

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

dist :: Pos -> Pos -> Int
dist = dot
  ( uncurry (+)
  . both (uncurry (-) . (uncurry max &&& uncurry min))
  . tt
  . both unPos
  )
  (,)

bfs :: Pos -> M.Map Cell [Pos] -> M.Map Pos Cell -> Sq.Seq Pos -> M.Map Pos Pos
bfs _      _   _ Sq.Empty      = error "Didn't find target!"
bfs target cps m (x Sq.:<| xs) = if target == x
  then M.empty
  else
    let
      srs =
        (concat . mapMaybe (\p -> fmap (go p) (M.lookup p m)) . allSurrounding)
          x
    in  M.union (M.fromList $ map (, x) srs)
                (bfs target cps (M.delete x m) (foldl' (Sq.|>) xs srs))
 where
  go p Empty      = [p]
  go p c@Portal{} = case fromJust (M.lookup c cps) of
    []  -> error "EMPTY LIST!"
    [x] -> [x]
    xs  -> filter (`M.member` m) $ allSurrounding $ head $ filter (/= p) xs

normalizeGameboard :: M.Map Pos Char -> [(Pos, Char)] -> M.Map Pos Cell
normalizeGameboard m =
  sort
    >>> findNeighbours
    >>> map
          (first
            ( head
            . map fst
            . filter ((== 1) . uncurry dist)
            . uncurry (liftA2 (,))
            . (id &&& concatMap (filter (`M.member` m) . allSurrounding))
            )
          )
    >>> M.fromList
    >>> M.union (M.map (const Empty) m)
 where
  -- | Find all neighbouring potential labels and group them
  -- Please give this a sorted list to get better time complexity and a correct answer
  findNeighbours :: [(Pos, Char)] -> [([Pos], Cell)]
  findNeighbours [] = []
  findNeighbours ((p, c) : xs) =
    let (xs', (p', c') : xs'') = break (((1 ==) . dist p) . fst) xs
    in  ([p, p'], Portal [c, c']) : findNeighbours (xs' ++ xs'')

parseBoard :: String -> (M.Map Pos Cell, M.Map Cell [Pos])
parseBoard =
  lines
    >>> zipWith (\y -> zipWith (\x c -> (Pos (y, x), c)) [0 ..]) [0 ..]
    >>> concat
    >>> filter (uncurry (||) . (isUpper &&& ('.' ==)) . snd)
    >>> partition (not . isUpper . snd)
    >>> (uncurry normalizeGameboard . first M.fromList)
    >>> (id &&& M.fromListWith (++) . map (second pure . swap) . M.toList)

main :: IO ()
main = do
  (pc, cps) <- second (M.delete Empty) . parseBoard <$> readFile "input.txt"
  let start  = fromJust $ M.lookup (Portal "AA") cps
  let target = head $ fromJust $ M.lookup (Portal "ZZ") cps
  let result = bfs target cps pc (Sq.fromList start)
  print $ M.size result

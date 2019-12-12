{-# LANGUAGE TupleSections #-}

import           Control.Arrow                            ( (***)
                                                          , (&&&)
                                                          )
import           Data.Function                            ( on )
import           Data.List                                ( break
                                                          , group
                                                          , groupBy
                                                          , maximumBy
                                                          , sort
                                                          , sortOn
                                                          )
import           Data.Ord                                 ( comparing )
import qualified Data.Sequence                 as Sq
import           Data.Sequence                            ( Seq(..)
                                                          , (|>)
                                                          )
import           Data.Tuple                               ( swap )

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

calcAngle :: (Double, Double) -> (Double, Double) -> Double
calcAngle p = uncurry (flip atan2) . both (uncurry (-)) . tt . (, p)

seenAsteroids
  :: (Double, Double) -> [(Double, Double)] -> ((Double, Double), Int)
seenAsteroids p = (p, ) . length . group . sort . map (calcAngle p)

toDouble :: Int -> Double
toDouble = fromIntegral

consume :: Int -> Seq [(Double, Double)] -> (Double, Double)
consume _ Empty      = error "This should never happen."
consume n (x :<| xs) = case horiz n x of
  (200, [x']) -> x'
  (n' , []  ) -> consume n' xs
  (n' , x'  ) -> consume n' (xs |> x')
 where
  horiz :: Int -> [(Double, Double)] -> (Int, [(Double, Double)])
  horiz n   []       = (n, [])
  horiz 200 (x : xs) = (200, [x])
  horiz n   (x : xs) = (succ n, xs)

main :: IO ()
main = do
  asteroids <-
    concat
    .   zipWith
          (\y ->
            map ((, toDouble y) . toDouble . fst) . filter ((== '#') . snd) . zip
              [0 ..]
          )
          [0 ..]
    .   lines
    <$> readFile "input.txt"

  -- Part 1
  let (station, seen) =
        maximumBy (comparing snd) $ map (`seenAsteroids` asteroids) asteroids
  print seen

  -- Part 2
  -- This list goes counter clockwise, we want to go clockwise, so we reverse the list.
  -- We also want to go from (0,1) and not (1,0) which atan2 does, so we start from pi/2
  -- instead of 0.
  print
    . (\(x, y) -> x * 100 + y)
    . consume 1
    . Sq.fromList
    . map (sortOn (\(x, y) -> sqrt $ x ** 2.0 + y ** 2.0) . map fst)
    . uncurry (++)
    . swap
    . break ((<= (pi / 2)) . snd . head)
    . reverse
    . groupBy (on (==) snd)
    . sortOn snd
    . map (id &&& calcAngle station)
    $ asteroids

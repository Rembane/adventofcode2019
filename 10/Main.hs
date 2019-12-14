{-# LANGUAGE TupleSections #-}

import           Control.Arrow                            ( (***)
                                                          , (&&&)
                                                          , second
                                                          )
import           Data.Function                            ( on )
import           Data.List                                ( group
                                                          , groupBy
                                                          , maximumBy
                                                          , sort
                                                          , sortOn
                                                          , uncons
                                                          )
import           Data.Maybe                               ( fromJust )
import           Data.Ord                                 ( comparing )
import qualified Data.Sequence                 as Sq
import           Data.Sequence                            ( Seq(..)
                                                          , (|>)
                                                          )

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

calcAngle :: (Double, Double) -> (Double, Double) -> Double
calcAngle p =
  norm
    . subtract (pi / 2)
    . uncurry (flip atan2)
    . second negate
    . both (uncurry (-))
    . tt
    . (, p)
  where norm v = if v < 0.0 then v + 2 * pi else v

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
  horiz m   []       = (m, [])
  horiz 200 (y : _ ) = (200, [y])
  horiz m   (_ : ys) = (succ m, ys)

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
  print station
  print seen

  -- Part 2
  print
    . (\(x, y) -> x * 100 + y)
    . consume 1
    . Sq.fromList
    . uncurry (:)
    . second reverse
    . fromJust
    . uncons
    . map
        ( sortOn
            (\(x, y) ->
              sqrt $ (x - fst station) ** 2.0 + (y - snd station) ** 2.0
            )
        . map fst
        )
    . groupBy (on (==) snd)
    . sortOn snd
    . map (id &&& calcAngle station)
    . filter (/= station)
    $ asteroids

{-# LANGUAGE TupleSections #-}

import           Control.Arrow                            ( (***)
                                                          , (&&&)
                                                          )
import           Data.Function                            ( on )
import           Data.List                                ( group
                                                          , groupBy
                                                          , maximumBy
                                                          , sort
                                                          , sortOn
                                                          )
import           Data.Ord                                 ( comparing )

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

seenAsteroids
  :: (Double, Double) -> [(Double, Double)] -> ((Double, Double), Int)
seenAsteroids p = (p, ) . length . group . sort . map
  (uncurry (flip atan2) . both (uncurry (-)) . tt . (, p))

toDouble :: Int -> Double
toDouble = fromIntegral

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
  let station =
        maximumBy (comparing snd) $ map (`seenAsteroids` asteroids) asteroids
  print station

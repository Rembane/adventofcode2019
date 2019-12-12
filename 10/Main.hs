{-# LANGUAGE TupleSections #-}

import           Control.Arrow                            ( (***) )
import           Data.List                                ( group
                                                          , sort
                                                          )

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

tt :: ((a, b), (a, b)) -> ((a, a), (b, b))
tt ((x1, y1), (x2, y2)) = ((x1, x2), (y1, y2))

seenAsteroids :: (Double, Double) -> [(Double, Double)] -> [[Double]]
seenAsteroids p =
  group . sort . map (uncurry (flip atan2) . both (uncurry (-)) . tt . (, p))

main :: IO ()
main = do
  asteroids <-
    concat
    .   zipWith
          (\y ->
            map
                ( (, (fromIntegral :: Int -> Double) y)
                . (fromIntegral :: Int -> Double)
                . fst
                )
              . filter ((== '#') . snd)
              . zip [0 ..]
          )
          [0 ..]
    .   lines
    <$> readFile "input.txt"
  print $ maximum $ map (length . (`seenAsteroids` asteroids)) asteroids

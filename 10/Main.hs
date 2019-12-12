{-# LANGUAGE TupleSections #-}

import           Control.Arrow                            ( (&&&)
                                                          , (***)
                                                          , second
                                                          )
import           Data.Foldable                            ( traverse_ )
import           Data.Ix                                  ( inRange )
import           Data.List                                ( break )
import           Data.Maybe                               ( maybeToList )
import           Data.Ratio                               ( Ratio
                                                          , (%)
                                                          , denominator
                                                          , numerator
                                                          )
import qualified Data.Set                      as S
import           Debug.Trace

left :: Int -> S.Set (Int, Int)
left i = S.fromList $ zip (repeat 0) [0 .. i]

top :: Int -> S.Set (Int, Int)
top i = S.fromList $ zip [0 .. i] (repeat 0)

bottom :: Int -> S.Set (Int, Int)
bottom i = S.fromList $ zip (repeat i) [0 .. i]

right :: Int -> S.Set (Int, Int)
right i = S.fromList $ zip [0 .. i] (repeat i)

chooseSides :: Int -> Int -> (Int, Int) -> S.Set (Int, Int)
chooseSides maxX maxY (x, y) =
  S.union (S.union (right maxX) (left maxX)) (S.union (top maxY) (bottom maxY))

{- S.union
  (if x <= maxX - x then right maxX else left maxX)
  (if y <= maxY - y then bottom maxY else top maxY) -}

createLine :: Int -> Int -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
createLine maxX maxY (x1, y1) (x2, y2) = case (k, m) of
  (Just k', Just m') ->
    ( map (numerator *** numerator)
      . filter
          ( uncurry (&&)
          . (inRange (0, maxY) . numerator &&& (== 1) . denominator)
          . snd
          )
      . map (id &&& (m' +) . (k' *))
      )
      ([0 .. maxX % 1] :: [Ratio Int])
  _ -> zip (repeat x1) [0 .. maxY]
 where
  deltaX = case x1 - x2 of
    0 -> Nothing
    v -> Just v
  k = fmap ((y1 - y2) %) deltaX
  m = fmap (negate . (* ((x1 % 1) - y1 % 1))) k

safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast [x     ] = Just x
safeLast (_ : xs) = safeLast xs

safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (x : xs) = xs

seenAsteroids
  :: Int -> Int -> (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
seenAsteroids maxX maxY p asteroids = S.fromList
  (concatMap
    ( uncurry (++)
    . (maybeToList . safeLast *** maybeToList . safeHead . safeTail)
    . break (== p)
    . filter (`S.member` asteroids)
    . createLine maxX maxY p
    )
    (S.toAscList (chooseSides maxX maxY p))
  )

main :: IO ()
main = do
  asteroids <-
    S.fromList
    .   concat
    .   zipWith (\y -> map ((, y) . fst) . filter ((== '#') . snd) . zip [0 ..])
                [0 ..]
    .   lines
    <$> readFile "input2.txt"
  let maxX = foldr (max . fst) 0 asteroids
  let maxY = foldr (max . snd) 0 asteroids
  traverse_ print
    $ foldr (\x -> ((x, seenAsteroids maxX maxY x asteroids) :)) [] asteroids


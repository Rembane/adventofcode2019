import           Data.Char                                ( digitToInt
                                                          , intToDigit
                                                          , isDigit
                                                          )
import qualified Data.Vector.Unboxed           as VU

-- | Run one phase of the fft
fft :: VU.Vector Int -> VU.Vector Int
fft xs = VU.map
  (\i -> (`mod` 10) . abs . VU.sum $ VU.zipWith
    (*)
    (VU.fromList $ take (VU.length xs) $ tail $ cycle $ concatMap
      (replicate i)
      [0, 1, 0, -1]
    )
    xs
  )
  (VU.enumFromN 1 (VU.length xs))

run :: Int -> VU.Vector Int -> VU.Vector Int
run 0 v = v
run i v = run (i - 1) (fft v)

main :: IO ()
main =
  putStrLn
    .   VU.toList
    .   VU.map intToDigit
    .   VU.take 8
    .   run 100
    .   VU.fromList
    .   map digitToInt
    .   filter isDigit
    =<< readFile "input.txt"

import           Control.Arrow                            ( (&&&) )
import           Data.Char                                ( digitToInt
                                                          , intToDigit
                                                          , isDigit
                                                          )

-- | Run one phase of the fft
fft :: [Int] -> [Int]
fft =
  zipWith
      (\i -> (`mod` 10) . abs . sum . zipWith
        (*)
        (tail $ cycle $ concatMap (replicate i) [0, 1, 0, -1])
      )
      [1 ..]
    . uncurry replicate
    . (length &&& id)

main =
  putStrLn
    .   map intToDigit
    .   take 8
    .   (!! 100)
    .   iterate fft
    .   map digitToInt
    .   filter isDigit
    =<< readFile "input.txt"

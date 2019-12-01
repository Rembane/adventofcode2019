import           Data.List                                ( iterate' )

elfAlgorithm :: Int -> Int
elfAlgorithm = subtract 2 . (`div` 3)

fuelAlgorithm :: Int -> Int
fuelAlgorithm = sum . tail . takeWhile (> 0) . iterate' elfAlgorithm

parter :: (Int -> Int) -> IO Int
parter f = sum . map (f . read) . lines <$> readFile "input.txt"

part1 :: IO Int
part1 = parter elfAlgorithm

part2 :: IO Int
part2 = parter fuelAlgorithm

main :: IO ()
main = part1 >>= print >> part2 >>= print

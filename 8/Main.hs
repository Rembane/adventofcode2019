import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Ord

-- 25 * 6 = 150 pixels per layer
layerize :: String -> [String]
layerize [] = []
layerize xs = uncurry (:) $ second layerize $ splitAt 150 xs

part1 :: [String] -> Int
part1 =
  product . tail . minimumBy (comparing head) . map (map length . group . sort)

part2 :: [String] -> String
part2 []   = []
part2 [ly] = ly
part2 (ly : lys) =
  zipWith (\c1 c2 -> if c1 == '2' then c2 else c1) ly (part2 lys)

main :: IO ()
main = do
  layers <- layerize . filter isDigit <$> readFile "input.txt"
  print $ part1 layers
  mapM_ print
    $ unfoldr
        (\s ->
          let (s', r) = splitAt 25 s
          in  if null s' then Nothing else Just (s', r)
        )
    $ map (\c -> if c == '0' then ' ' else c) (part2 layers)

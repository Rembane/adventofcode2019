import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Ord

-- 25 * 6 = 150 pixels per layer
layerize :: String -> [String]
layerize [] = []
layerize xs = uncurry (:) $ second layerize $ splitAt 150 xs

main =
  print
    .   product
    .   tail
    .   minimumBy (comparing head)
    .   map (map length . group . sort)
    .   layerize
    .   filter isDigit
    =<< readFile "input.txt"

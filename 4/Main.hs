import           Data.Char                                ( digitToInt )
import           Data.List                                ( foldl'
                                                          , group
                                                          )
doubleDigit :: String -> Bool
doubleDigit = any ((== 2) . length) . group

neverDecrease :: String -> Bool
neverDecrease = snd . foldl'
  (\(pre, b) c -> let i = digitToInt c in (i, i >= pre && b))
  (0, True)

main = print $ length $ filter
  (\i -> let s = show i in doubleDigit s && neverDecrease s)
  [168630 .. 718098]

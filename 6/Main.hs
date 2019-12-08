import           Control.Applicative                      ( (<|>)
                                                          , liftA2
                                                          )
import           Control.Arrow                            ( second )
import qualified Data.Map.Strict               as M

part1 :: M.Map String String -> Int
part1 m = sum $ map walk (M.keys m)
  where walk = maybe 0 ((1 +) . walk) . (`M.lookup` m)

dfs :: String -> String -> M.Map String [String] -> Maybe Int
dfs needle cur m
  | M.null m
  = Nothing
  | needle == cur
  = Just 0
  | otherwise
  = M.lookup cur m
    >>= foldr
          (\k n ->
            let r = (+ 1) <$> dfs needle k (M.delete cur m)
            in  liftA2 min n r <|> n <|> r
          )
          Nothing

part2 :: M.Map String [String] -> Maybe Int
part2 = dfs "SAN" "YOU"

main :: IO ()
main = do
  orbits <- map (second tail . break (== ')')) . lines <$> readFile "input.txt"
  print (part1 $ foldr (\(k, v) m -> M.insert v k m) M.empty orbits)
  print
    (part2 $ foldr
      (\(k, v) m -> M.insertWith (++) k [v] (M.insertWith (++) v [k] m))
      M.empty
      orbits
    )

import qualified Data.Map.Strict               as M

part1 :: M.Map String String -> Int
part1 m = sum $ map walk (M.keys m)
  where walk = maybe 0 ((1 +) . walk) . (`M.lookup` m)

main :: IO ()
main =
  print
    .   part1
    .   foldr (\x m -> let (k, v) = break (== ')') x in M.insert (tail v) k m)
              M.empty
    .   lines
    =<< readFile "input.txt"

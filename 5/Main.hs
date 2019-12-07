{-# LANGUAGE LambdaCase #-}
import           Control.Arrow                            ( (>>>) )
import           Control.Monad                            ( (>=>) )
import           Data.Function                            ( (&) )
import           Data.Maybe
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

newtype Parameters = Parameters {unParam :: (Bool, Bool)}

parseParameters :: Int -> Parameters
parseParameters = (`div` 100) >>> \case
  00 -> Parameters (False, False)
  01 -> Parameters (False, True)
  10 -> Parameters (True, False)
  11 -> Parameters (True, True)
  i  -> error $ "I do not recognize this: " <> show i

doubleRead :: VU.Vector Int -> Int -> Int
doubleRead v = (v VU.!) . (v VU.!)

readParam :: Bool -> VU.Vector Int -> Int -> Int
readParam isImmediate v i = if isImmediate then v VU.! i else doubleRead v i

binHelper
  :: (Int -> Int -> Int) -> Parameters -> VU.Vector Int -> Int -> VU.Vector Int
binHelper f ps v pc =
  let r = f (readParam (snd $ unParam ps) v (pc + 1))
            (readParam (fst $ unParam ps) v (pc + 2))
  in  VU.modify (\v' -> VUM.write v' (v VU.! (pc + 3)) r) v

eval :: Int -> VU.Vector Int -> IO (VU.Vector Int)
eval pc v =
  let
    i  = v VU.! pc
    ps = parseParameters i
  in
    putStrLn
        ("pc: " <> show pc <> " i: " <> show i <> " next ones: " <> show
          (fromMaybe [] (traverse (v VU.!?) [pc + 1, pc + 2, pc + 3]))
        )
      >> case mod i 100 of
           1 -> binHelper (+) ps v pc & eval (pc + 4)
           2 -> binHelper (*) ps v pc & eval (pc + 4)
           3 -> do
             putStrLn "Please give input"
             x <- read <$> getLine
             putStrLn $ mconcat
               ["Writing ", show x, " to position ", show (v VU.! (pc + 1))]
             VU.modify (\v' -> VUM.write v' (v VU.! (pc + 1)) x) v
               & eval (pc + 2)
           4 -> do
             putStrLn $ "Output: " <> show (v VU.! (pc + 1))
             eval (pc + 2) v
           99 -> pure v
           i' -> error $ "Nope: " <> show i'

part1 :: VU.Vector Int -> IO ()
part1 = eval 0 >=> print

parse :: String -> VU.Vector Int
parse = VU.fromList . p'
 where
  p' [] = []
  p' cs =
    let (i, rest) = span (/= ',') cs in read i : p' (dropWhile (== ',') rest)

main :: IO ()
main = do
  program <- parse <$> readFile "input.txt"
  part1 program

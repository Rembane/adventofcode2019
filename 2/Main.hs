{-# LANGUAGE LambdaCase #-}
import           Control.Arrow                            ( (***) )
import           Data.List                                ( break
                                                          , uncons
                                                          , unfoldr
                                                          )
import qualified Data.Vector.Unboxed           as VU

doubleRead :: _
doubleRead v = VU.read v >=> VU.read v

binHelper :: (Int -> Int -> Int) -> _ -> Int -> _
binHelper f v pc = (f <$> doubleRead v (pc + 1) <*> doubleRead v (pc + 2))
  >>= \r -> VU.read v (pc + 2) >>= \dest -> VU.write v dest r

runInstruction :: _
runInstruction v pc =
  VU.read v pc
    >>= \case
          1  -> binHelper (+) v pc
          2  -> binHelper (*) v pc
          99 -> error "DONE!"
    >>= (`runInstruction` (pc + 4))

main = do
  program <-
    VU.fromList
    .   unfoldr
          (sequence . ((read :: String -> Int) *** fmap snd . uncons) . break
            (== ',')
          )
    <$> readFile "input.txt"
  print program

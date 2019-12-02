{-# LANGUAGE LambdaCase #-}
import           Control.Arrow                            ( (***) )
import           Control.Monad                            ( (>=>) )
import           Control.Monad.Primitive                  ( PrimMonad
                                                          , PrimState
                                                          )
import           Data.List                                ( break
                                                          , uncons
                                                          , unfoldr
                                                          )
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM

doubleRead :: PrimMonad m => VUM.MVector (PrimState m) Int -> Int -> m Int
doubleRead v = VUM.read v >=> VUM.read v

binHelper
  :: PrimMonad m
  => (Int -> Int -> Int)
  -> VUM.MVector (PrimState m) Int
  -> Int
  -> m ()
binHelper f v pc = (f <$> doubleRead v (pc + 1) <*> doubleRead v (pc + 2))
  >>= \r -> VUM.read v (pc + 3) >>= \dest -> VUM.write v dest r

eval :: PrimMonad m => Int -> VUM.MVector (PrimState m) Int -> m ()
eval pc v = VUM.read v pc >>= \case
  1  -> binHelper (+) v pc >> eval (pc + 4) v
  2  -> binHelper (*) v pc >> eval (pc + 4) v
  99 -> pure ()
  i  -> error $ "Nope: " <> show i

main = do
  program <-
    VU.modify (eval 0)
    .   VU.modify (\v -> VUM.write v 1 12 >> VUM.write v 2 2)
    .   VU.fromList
    .   unfoldr (sequence . (read *** fmap snd . uncons) . break (== ','))
    <$> readFile "input.txt"
  print program

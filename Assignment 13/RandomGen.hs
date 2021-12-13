--Ege Sari s1034535
--Group 81

module RandomGen where

import Control.Monad
import System.Random
import RandomState


--randomRIO :: (Integer,Integer) -> IO Integer
--getStdGen::IO StdGen
--setStdGen::StdGen -> IO()
--get::RandomState StdGen
--put::StdGen -> RandomState()
--type GlobalState = StdGen
--randomR :: (Integer,Integer) → (StdGen→ (Integer,StdGen))
--newtype RandomState a = St { runState :: GlobalState -> (a, GlobalState) }

-- (>>=) :: (m a) -> (a-> mb) -> mb
-- (>>=) :: (RandomState StdGen) -> (StdGen-> RandomState Integer) -> RandomState Integer
genRandInteger :: (Integer,Integer) -> RandomState Integer
genRandInteger (a,b) = do
  (get) >>= (\s -> do
    let new = ((randomR (a,b))s)
    put (snd new)
    return (fst new)) 


roll_2d6 :: RandomState Integer
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6)
  return (a+b)
--fmap :: (a-> b) -> fa -> fb
-- fmap ::(a->b) -> IO a -> IO b
-- (>>=) :: (m a) -> (a-> mb) -> mb
-- (IO a) -> (a -> IO b) -> IO b
safeR :: RandomState a -> IO a
safeR m = do
  getStdGen >>= (\s -> do
    let new =((runState m) s)
    setStdGen (snd new)
    return (fst new) )

--these definitions can be used to test your function a bit more thoroughly
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer]
testme = randomN (0,999) 100 (mkStdGen 42)

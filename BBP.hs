{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BBP where

import HighPerformance (effSum)
import Math.NumberTheory.Concrete (powerMod)
import Math.Progressions (Frac(..))
import Rio

data Constant = Catalan | Log2 | Pi deriving Show

data Bin
data Hex

bbpBin :: Constant -> Integer -> Int
bbp Log2 d = 1
  where
    log2BBP :: (Int,Int,Int)
    log2BBP = (2,1,1)
    func k = (powerMod 2 (d-k) k) :/ k
    fracPart = sum $ map func [1..d]
bbp Pi d = 1
  where
    piBBP :: (Int,Int,(Int,Int),(Int,Int),(Int,Int),(Int,Int))
    piBBP =  (16,8,(4,1),(-2,4),(-1,5),(-1,6))

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Yahp.FinancialUnits where

import Data.List
import Data.Ord
import Data.String
import Data.Text (Text)
import Data.Word
import TextShow

data Unit = Percent
          | BasisPoint
          | Thousand
          | None
          | Million
          | Billion
          | Trillion
            deriving (Enum, Eq, Bounded, Show, Ord)

-- | List of financial units
financialUnitDescending :: [Unit]
financialUnitDescending = sortBy (flip $ comparing $ multiplier @Double ) [minBound..]

multiplier :: (Fractional a, Num a) => Unit -> a
multiplier None             = 1
multiplier Percent          = 10 ^^ (-2 :: Int)
multiplier BasisPoint       = 10 ^^ (-4 :: Int)
multiplier Thousand         = 10 ^^ ( 3 :: Int)
multiplier Million          = 10 ^^ ( 6 :: Int)
multiplier Billion          = 10 ^^ ( 9 :: Int)
multiplier Trillion         = 10 ^^ (12 :: Int)

symbol :: IsString a => Unit -> a
symbol BasisPoint       = "bps"
symbol Percent          = "%"
symbol Thousand         = "k"
symbol Million          = "m"
symbol Billion          = "b"
symbol Trillion         = "t"
symbol None             = ""
  



-- | this function takes care to behaive like the finance world expects
--
-- Examples:
-- 
-- λ> showFinancialValue @Double 0 999.85e3
-- "1m"
-- λ> showFinancialValue @Double 1 999.95e3
-- "1.0m"
-- λ> showFinancialValue @Double 1 999.85e3
-- "999.9k"
--
-- using Prelude.round would result in:
-- 
-- λ> showFinancialValue @Double 1 999.85e3
-- "999.8k"
--
-- more examples
-- λ> mapM_ (\x -> let y = (10^^x) * 98.5 :: Double in print $ (y, showFinancialValue 1 y)) [-10..13::Int]
-- (9.85e-9,"0.0bps")
-- (9.85e-8,"0.0bps")
-- (9.85e-7,"0.0bps")
-- (9.849999999999999e-6,"0.1bps")
-- (9.85e-5,"1.0bps")
-- (9.85e-4,"9.9bps")
-- (9.850000000000001e-3,"98.5bps")
-- (9.85e-2,"9.9%")
-- (0.985,"98.5%")
-- (9.850000000000001,"9.9")
-- (98.5,"98.5")
-- (985.0,"985.0")
-- (9850.0,"9.9k")
-- (98500.0,"98.5k")
-- (985000.0,"985.0k")
-- (9850000.0,"9.9m")
-- (9.85e7,"98.5m")
-- (9.85e8,"985.0m")
-- (9.85e9,"9.9b")
-- (9.85e10,"98.5b")
-- (9.85e11,"985.0b")
-- (9.85e12,"9.9t")
-- (9.85e13,"98.5t")
-- (9.85e14,"985.0t")
-- 
-- λ> mapM_ (\x -> let y = (10^^x) * 999.85 :: Double in print $ (y, showFinancialValue 1 y)) [-10..13::Int]
-- (9.9985e-8,"0.0bps")
-- (9.9985e-7,"0.0bps")
-- (9.998500000000001e-6,"0.1bps")
-- (9.9985e-5,"1.0bps")
-- (9.9985e-4,"10.0bps")
-- (9.9985e-3,"1.0%")
-- (9.9985e-2,"10.0%")
-- (0.99985,"1.0")
-- (9.9985,"10.0")
-- (99.98500000000001,"100.0")
-- (999.85,"999.9")
-- (9998.5,"10.0k")
-- (99985.0,"100.0k")
-- (999850.0,"999.9k")
-- (9998500.0,"10.0m")
-- (9.9985e7,"100.0m")
-- (9.9985e8,"999.9m")
-- (9.9985e9,"10.0b")
-- (9.9985e10,"100.0b")
-- (9.9985e11,"999.9b")
-- (9.9985e12,"10.0t")
-- (9.9985e13,"100.0t")
-- (9.9985e14,"999.9t")
-- (9.9985e15,"9998.5t")
showFinancialValue :: (TextShow a, Ord a, RealFrac a) => Word8 -> a -> Text
showFinancialValue decimals val = foldr g (pick $ last scaledValues)
  $ zip scaledValues $ tail scaledValues
  where g (scaled@(m1,_,_), (m2, (v2, _), _)) continue = if abs v2 * m2 >= m1 then pick scaled else continue
        pick (_, (_, sLast), uLast) = sLast <> symbol uLast
        scaledValues = (\u -> let m = multiplier u in (m, scaledValue decimals $ val / m, u))
          <$> financialUnitDescending
{-# INLINABLE showFinancialValue #-}
{-# SPECIALISE showFinancialValue :: Word8 -> Double -> Text #-}
{-# SPECIALISE showFinancialValue :: Word8 -> Float -> Text #-}

scaledValue :: (TextShow a, RealFrac a) => Word8 -> a -> (a, Text)
scaledValue decimals v | decimals == 0 = let r = roundAwayFrom0 v in (fromIntegral r, showt @Int r) 
                       | True = let r = fromIntegral @Int (roundAwayFrom0 $ v * scale) / scale
                               in (r, showt r)
  where scale = 10^decimals
{-# INLINABLE scaledValue #-}


roundAwayFrom0 :: (RealFrac a, Integral b) => a -> b
roundAwayFrom0 x | x > 0 && re >= 0.5   = int + 1
                 | x < 0 && re <= -0.5  = int - 1
                 | True                 = int
  where (int,re) = properFraction x
{-# INLINABLE roundAwayFrom0 #-}
{-# SPECIALISE roundAwayFrom0 :: Double -> Int #-}
{-# SPECIALISE roundAwayFrom0 :: Float -> Int #-}

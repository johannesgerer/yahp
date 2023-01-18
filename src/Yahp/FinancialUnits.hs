{-# LANGUAGE BlockArguments #-}
module Yahp.FinancialUnits where

import           Data.Bool
import           Data.Int
import           Data.List
import           Data.Ord
import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           GHC.Float
import           GHC.TypeLits
import           Prelude
import           TextShow


-- | this can be used in `deriving via` 
--
-- @
--  newtype A = A Double deriving (Show, TextShow) via FinancialUnits 2 Double
-- @
newtype FinancialUnits (decimals :: Nat) a = FinancialUnits a

-- | this can be used in `deriving via` 
--
-- @
--  newtype A = A Double deriving (Show, TextShow) via ThousandsSeparators 2 Double
-- @
newtype ThousandsSeparators (decimals :: Nat) a = ThousandsSeparators a

instance (KnownNat decimals, TextShow a, Fractional a, Roundable a) => TextShow (FinancialUnits decimals a) where
  showb (FinancialUnits v) = fromText $ showFinancialUnits (fromIntegral $ natVal $ Proxy @decimals) v

instance (KnownNat decimals, TextShow a, Fractional a, Roundable a) => Show (FinancialUnits decimals a) where
  show = T.unpack . showt 

instance (KnownNat decimals, TextShow a, Roundable a) => TextShow (ThousandsSeparators decimals a)
  where
  showb (ThousandsSeparators v) = fromText $
    showThousandsSeparatorUS (fromIntegral $ natVal $ Proxy @decimals) v

instance (KnownNat decimals, TextShow a, Roundable a) => Show (ThousandsSeparators decimals a) where
  show = T.unpack . showt 


-- | same problem as the other functions (see comments of showFinancialUnits and roundAwayFrom0)
-- 
-- λ> showThousandsSeparatorUS @Double 4 8.77705
-- "8.7770" instead of "8.7771"
showThousandsSeparatorUS :: (TextShow a, Roundable a) => Int -> a -> Text
showThousandsSeparatorUS = showThousandsSeparator ',' '.'

-- | Copyright (c) 2017 Alexander Thiemann
showThousandsSeparator :: (TextShow a, Roundable a) => Char -> Char -> Int -> a -> Text
showThousandsSeparator pc_thousandsSep pc_decimalSep pc_decimals n =
  let tpow = 10 ^ pc_decimals
      lshift = n * fromIntegral tpow
      lshiftr = roundAwayFrom0 lshift
      lshifti' = abs lshiftr
      intPart = lshifti' `div` tpow
      decPart = lshifti' - intPart * tpow
      preDecimal =
          if lshiftr < 0
          then prettyI pc_thousandsSep (intPart * (-1))
          else prettyI pc_thousandsSep intPart
      postDecimal =
          if pc_decimals > 0
          then T.cons pc_decimalSep (T.justifyRight pc_decimals '0' $ T.pack $ show decPart)
          else ""
  in bool (showt n) (preDecimal <> postDecimal) $ isFinite n

-- | Copyright (c) 2017 Alexander Thiemann
prettyI :: Char -> Int -> T.Text
prettyI s n =
  let ni = T.pack $ show $ abs n
      nis = T.intercalate (T.singleton s) $ reverse $ map T.reverse $ T.chunksOf 3 $ T.reverse ni
  in if n < 0 then "-" <> nis else nis

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

multiplier :: (Fractional a) => Unit -> a
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
-- λ> showFinancialUnits @Double 0 999.85e3
-- "1m"
-- λ> showFinancialUnits @Double 1 999.95e3
-- "1.0m"
-- λ> showFinancialUnits @Double 1 999.85e3
-- "999.9k"
--
-- using Prelude.round would result in:
-- 
-- λ> showFinancialUnits @Double 1 999.85e3
-- "999.8k"
--
-- more examples
-- λ> mapM_ (\x -> let y = (10^^x) * 98.5 :: Double in print $ (y, showFinancialUnits 1 y)) [-10..13::Int]
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
-- λ> mapM_ (\x -> let y = (10^^x) * 999.85 :: Double in print $ (y, showFinancialUnits 1 y)) [-10..13::Int]
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
--
-- TODO: make this function work correctly for (up to 15-digit) decimals
-- λ> showFinancialUnits @Double 4 8777.05
-- "8.777k"
-- λ> showFinancialUnits @Double 2 1.005
-- "1.0"
--
-- look at the Show instance of Double, do something similar (use floatToDigits directly)
-- https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.Float.html#floatToDigits
--
showFinancialUnits :: (TextShow a, Fractional a, Roundable a) => Word8 -> a -> Text
showFinancialUnits decimals val = bool (showt val) (foldr g (pick $ last scaledValues)
  $ zip scaledValues $ tail scaledValues) $ isFinite val
  where g (scaled@(m1,_,_), (m2, (v2, _), _)) continue = if abs v2 * m2 >= m1 then pick scaled else continue
        pick (_, (_, sLast), uLast) = sLast <> symbol uLast
        scaledValues = (\u -> let m = multiplier u in (m, scaledValue decimals $ val / m, u))
          <$> financialUnitDescending
{-# INLINABLE showFinancialUnits #-}
{-# SPECIALISE showFinancialUnits :: Word8 -> Double -> Text #-}
{-# SPECIALISE showFinancialUnits :: Word8 -> Float -> Text #-}

scaledValue :: (TextShow a, Fractional a, Roundable a) => Word8 -> a -> (a, Text)
scaledValue decimals v | decimals == 0 = let r = roundAwayFrom0 v in (fromIntegral r, showt @Int r) 
                       | True = let r = fromIntegral @Int (roundAwayFrom0 $ v * scale) / scale
                               in (r, showt r)
  where scale = 10^decimals
{-# INLINABLE scaledValue #-}

class (Num a, Ord a) => Roundable a where
  isFinite :: a -> Bool
  isFinite _ = True
  roundAwayFrom0 :: a -> Int

instance {-# OVERLAPS #-} Roundable Int         where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Int64       where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Int32       where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Int16       where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Word        where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Word32      where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Word64      where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Integer     where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Natural     where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Int8        where roundAwayFrom0 = fromIntegral
instance {-# OVERLAPS #-} Roundable Word8       where roundAwayFrom0 = fromIntegral
  
instance {-# OVERLAPPABLE #-} (Ord a, Num a, RealFloat a) => Roundable a where
  isFinite x = not $ isNaN x || isInfinite x
  roundAwayFrom0 = roundAwayFrom0_


-- | this function works correctly, but not when used on scaled double, i.e. 8.77705 * 10000, 1.005 * 100,
-- so it is not correctly used in `showFinancialUnits` (see comments there)
--
-- modified from https://hackage.haskell.org/package/base-4.18.0.0/docs/src/GHC.Float.html#roundFloat
roundAwayFrom0_ :: (RealFrac a, Integral b) => a -> b
roundAwayFrom0_ x = case properFraction x of
                     (n,r) -> let
                                m         = if r < 0.0 then n - 1 else n + 1
                                half_down = abs r - 0.5
                              in
                              case (compare half_down 0.0) of
                                LT -> n
                                -- EQ -> if even n then n else m
                                _ -> m
{-# SPECIALISE roundAwayFrom0 :: Double -> Int #-}
{-# SPECIALISE roundAwayFrom0 :: Float -> Int #-}

-- | mapM_ print $ (\x p -> (p,x,roundToMultiplesOf @Int Proxy p 10 x)) <$> ([90, 93, 95, 97, 100:: Double] <> fmap negate [90, 93, 95, 97, 100]) <*> [True, False]
-- [90.0,90.0,100.0,100.0,100.0,-90.0,-90.0,-90.0,-100.0,-100.0,90.0,90.0,90.0,100.0,100.0,-90.0,-90.0,-100.0,-100.0,-100.0]
-- (True,90.0,90.0)
-- (False,90.0,90.0)
-- (True,93.0,90.0)
-- (False,93.0,90.0)
-- (True,95.0,100.0)
-- (False,95.0,90.0)
-- (True,97.0,100.0)
-- (False,97.0,100.0)
-- (True,100.0,100.0)
-- (False,100.0,100.0)
-- (True,-90.0,-90.0)
-- (False,-90.0,-90.0)
-- (True,-93.0,-90.0)
-- (False,-93.0,-90.0)
-- (True,-95.0,-90.0)
-- (False,-95.0,-100.0)
-- (True,-97.0,-100.0)
-- (False,-97.0,-100.0)
-- (True,-100.0,-100.0)
-- (False,-100.0,-100.0)
roundToMultiplesOf :: forall i r . (RealFrac r, Num r, Integral i)
  => Proxy i -- ^ internal integral type used in properFraction.
  -- for double use @Integer, because that is used in the properFractionDouble implementatino
  -> Bool -- ^ round to plus (or minus) infinity
  -> r -- ^ to multiples of this number
  -> r -- ^ number to be rounded
  -> r
roundToMultiplesOf _ posInf size input = size * fromIntegral @i unitsRounded
  where (units, re) = properFraction $ input / size
        (op1, op2) = if posInf then ((>=),(>)) else ((>),(>=))
        unitsRounded =  if re `op1` 0.5 then units + 1
                        else if (-0.5) `op2` re then units - 1
                             else units


-- nextafter1 :: RealFloat t => t -> t
-- nextafter1 0 = 0
-- nextafter1 x | GHC.Float.isNaN x = x
-- nextafter1 x | GHC.Float.isInfinite x = x
-- nextafter1 x = try (abs x)
--   where try d = let d1 = d/2
--                 in if x + d1 == x then improve d1 d else try d1
--         improve a b = let middle = (a+b)/2
--                       in if middle == b || middle == a 
--                          then x + b
--                          else if x + middle > x 
--                               then improve a middle 
--                               else improve middle b

-- -- digits10 :: (RealFloat a) => a -> Int
-- digits10 x = count 0 (floatRadix x ^ floatDigits x)
--   where count n v = if v < 10 then n else count (n+1) (v `quot` 10)

-- sd x = show @Integer $ round $ 1000000000000000000000000000000000000000000000000000000000 * toRational @Double x

-- -- | A class that let's you work on both Integer and RealFrac types for the purpose of functions in this
-- -- module
-- class (Num a, RealFrac (Frac a)) => ToFrac a where
--   type Frac a
--   toFrac :: a -> Frac a
--   toFracRoundAwayFrom0 :: Integral b => a -> b

--   type Frac a = a
--   default toFrac :: Real a => a -> Frac a
--   toFrac = realToFrac

--   default toFracRoundAwayFrom0 :: (Integral b, Integral a) => a -> b
--   toFracRoundAwayFrom0 = fromIntegral

-- instance               ToFrac WordPtr where type Frac WordPtr = Double
-- instance               ToFrac IntPtr where type Frac IntPtr = Double
-- instance               ToFrac Int8 where type Frac Int8 = Double
-- instance               ToFrac Int64 where type Frac Int64 = Double
-- instance               ToFrac Int32 where type Frac Int32 = Double
-- instance               ToFrac Int16 where type Frac Int16 = Double
-- instance               ToFrac Word where type Frac Word = Double
-- instance               ToFrac Integer where type Frac Integer = Double
-- instance               ToFrac Int where type Frac Int = Double
-- instance               ToFrac Word8 where type Frac Word8 = Double
-- instance               ToFrac Word64 where type Frac Word64 = Double
-- instance               ToFrac Word32 where type Frac Word32 = Double
-- instance               ToFrac Word16 where type Frac Word16 = Double

-- instance   ToFrac a => ToFrac (Down a) where
--   type Frac (Down a) = Down (Frac a)
--   toFrac = Down . toFrac . getDown
--   toFracRoundAwayFrom0 = toFracRoundAwayFrom0 . getDown

-- instance               ToFrac Float where toFracRoundAwayFrom0 = roundAwayFrom0
-- instance               ToFrac Double where toFracRoundAwayFrom0 = roundAwayFrom0
-- instance Integral a => ToFrac (Ratio a) where toFracRoundAwayFrom0 = roundAwayFrom0

-- integerRoundAwayFrom0 n removeDigits = if n > 0 then
--                                          if r * 10 >= 5 * sc then q + 1
--                                          else q
--                                        else if n < 0 && r * 10 <= - 5 * sc then q - 1
--                                             else q
--   where (q,r) = quotRem n sc
--         sc = 10^removeDigits


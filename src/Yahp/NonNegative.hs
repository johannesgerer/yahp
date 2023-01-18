{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Yahp.NonNegative where

import           Data.Aeson
import           Prelude ()
import qualified Yahp as Y
import           Yahp hiding (abs, unzip, properFraction, quotRem, (+), (-), (*), (/), sum, min, max, zero)


newtype NonNegative v = UnsafeNonNegative { unsafeFromNonNegative :: v }
  deriving (Generic, ToJSON, Eq, Ord, Hashable)

instance Show v => Show (NonNegative v) where
  show x = "NonNegative " <> show (fromNonNegative x)


instance (Show a, Typeable a, Ord a, Num a, FromJSON a) => FromJSON (NonNegative a) where
  parseJSON = either fail pure . nonNegativeError "" <=< parseJSON

-- * Debugging

traceNegative :: (Ord a, Num a, Show a) => NonNegative a -> NonNegative a
traceNegative x = if fromNonNegative x < 0 then traceShow ("This should not be negative: " <> show x) x else x
{-# WARNING traceNegative "'traceNegative' remains in code" #-}

-- * Destruction

fromNonNegative :: NonNegative v -> v
fromNonNegative = unsafeFromNonNegative
{-# INLINE fromNonNegative #-}

-- * Construction

abs :: Num a => a -> NonNegative a
abs = UnsafeNonNegative . Y.abs
{-# INLINE abs #-}

nonNegativeError :: (Show a, Typeable a, Ord a, Num a, MonadError s m, Semigroup s, ConvertText String s)
  => s -> a -> m (NonNegative a)
nonNegativeError msg x = withNonNegative pure
  (\_ -> throwError $ msg <> (toS $ "Encountered negative " <> show (typeOf x) <> "-value: " <> show x))
  x
{-# INLINABLE nonNegativeError #-}

withNonNegative :: (Ord a, Num a) => (NonNegative a -> b) -- ^ for positive inputs
        -> (NonNegative a -> b) -- ^ for negative inputs
        -> a -> b
withNonNegative f g x = if x >= 0 then f $ UnsafeNonNegative x else g $ UnsafeNonNegative $ negate x
{-# INLINABLE withNonNegative #-}

nonNegativeEither :: (Ord a, Num a) => a -> Either (NonNegative a) -- ^ positive inputs
                     (NonNegative a) -- ^ negative inputs
nonNegativeEither = withNonNegative Left Right
{-# INLINABLE nonNegativeEither #-}

-- * Helpers

unsafeMap :: (a -> b) -> NonNegative a -> NonNegative b
unsafeMap f (UnsafeNonNegative v) = UnsafeNonNegative $ f v
{-# INLINE unsafeMap #-}

unsafeMap2 :: Functor f => (f a -> b) -> f (NonNegative a) -> NonNegative b
unsafeMap2 f = UnsafeNonNegative . f . fmap fromNonNegative
{-# INLINE unsafeMap2 #-}

unsafeLift2 :: (a -> b -> c) -> NonNegative a -> NonNegative b -> c
unsafeLift2 f (UnsafeNonNegative a) (UnsafeNonNegative b) =  f a b
{-# INLINE unsafeLift2 #-}

unsafeLift2_ :: (a -> b -> c) -> NonNegative a -> NonNegative b -> NonNegative c
unsafeLift2_ = fmap2 UnsafeNonNegative . unsafeLift2
{-# INLINE unsafeLift2_ #-}

unzip :: NonNegative (a,b) -> (NonNegative a, NonNegative b)        
unzip (UnsafeNonNegative (a,b)) = (UnsafeNonNegative a, UnsafeNonNegative b)
{-# INLINE unzip #-}

fromIntegral :: (Integral a, Num b) => NonNegative a -> NonNegative b
fromIntegral = unsafeMap Y.fromIntegral
{-# INLINE fromIntegral #-}

-- * Operations

(+) :: Num a => NonNegative a -> NonNegative a -> NonNegative a
(+) = unsafeLift2_ (Y.+)
{-# INLINE (+) #-}

(-) :: Num a => NonNegative a -> NonNegative a -> a
(-) = unsafeLift2 (Y.-)
{-# INLINE (-) #-}

(*) :: Fractional a => NonNegative a -> NonNegative a -> NonNegative a
(*) = unsafeLift2_ (Y.*)
{-# INLINE (*) #-}

(/) :: Fractional a => NonNegative a -> NonNegative a -> NonNegative a
(/) = unsafeLift2_ (Y./)
{-# INLINE (/) #-}

positivePart :: (Ord a, Num a) => a -> NonNegative a
positivePart = UnsafeNonNegative . Y.max 0
{-# INLINE positivePart #-}

negativePart :: (Ord a, Num a) => a -> NonNegative a
negativePart = positivePart . negate
{-# INLINE negativePart #-}

sum :: Num a => [NonNegative a] -> NonNegative a
sum = unsafeMap2 Y.sum
{-# INLINE sum #-}

properFraction :: (RealFrac a, Integral b) => NonNegative a -> (NonNegative b, NonNegative a)
properFraction = unzip . unsafeMap Y.properFraction
{-# INLINE properFraction #-}

quotRem :: Integral a => NonNegative a -> NonNegative a -> (NonNegative a, NonNegative a)
quotRem = fmap unzip . unsafeLift2_ Y.quotRem
{-# INLINE quotRem #-}

min :: Ord a => NonNegative a -> NonNegative a -> NonNegative a
min = unsafeLift2_ Y.min
{-# INLINE min #-}

max :: Ord a => NonNegative a -> NonNegative a -> NonNegative a
max = unsafeLift2_ Y.max
{-# INLINE max #-}


mininum :: Ord a => NE (NonNegative a) -> NonNegative a
mininum = unsafeMap2 Y.minimum
{-# INLINE mininum #-}

maximum :: Ord a => NE (NonNegative a) -> NonNegative a
maximum = unsafeMap2 Y.maximum
{-# INLINE maximum #-}

zero :: Num a => NonNegative a
zero = UnsafeNonNegative 0
{-# INLINE zero #-}


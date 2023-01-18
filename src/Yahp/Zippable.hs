{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
      
module Yahp.Zippable where


import           Control.Arrow
import           Control.Monad.Writer (WriterT(..))
import           Data.Coerce
import           Data.Function (id)
import           Data.List
import           Data.SOP (I(..), (:.:)(..), unComp)
import qualified Data.Vector as V
import           Protolude hiding ((:.:))

-- * Zippable class

class Functor f => Zippable f where
  zipWith_      :: (a -> b -> x)                        -> f a -> f b -> f x
  zipWith3_     :: (a -> b -> c -> x)                   -> f a -> f b -> f c -> f x
  zipWith4_     :: (a -> b -> c -> d -> x)              -> f a -> f b -> f c -> f d -> f x
  zipWith5_     :: (a -> b -> c -> d -> e -> x)         -> f a -> f b -> f c -> f d -> f e -> f x
  zipWith6_     :: (a -> b -> c -> d -> e -> g -> x)    -> f a -> f b -> f c -> f d -> f e -> f g -> f x

  default zipWith_ :: Coercible (f a -> f b -> f x) (a -> b -> x) => (a -> b -> x) -> f a -> f b -> f x
  zipWith_ = coerce
  {-# INLINE zipWith_ #-}

  default zipWith3_ :: Coercible (f a -> f b -> f c -> f x) (a -> b -> c -> x)
    => (a -> b -> c -> x)                -> f a -> f b -> f c -> f x
  zipWith3_ = coerce
  {-# INLINE zipWith3_ #-}

  default zipWith4_ :: Coercible (f a -> f b -> f c -> f d -> f x) (a -> b -> c -> d -> x)
    => (a -> b -> c -> d -> x)                -> f a -> f b -> f c -> f d -> f x
  zipWith4_ = coerce
  {-# INLINE zipWith4_ #-}

  default zipWith5_ :: Coercible (f a -> f b -> f c -> f d -> f e -> f x) (a -> b -> c -> d -> e -> x)
    => (a -> b -> c -> d -> e -> x) -> f a -> f b -> f c -> f d -> f e -> f x
  zipWith5_ = coerce
  {-# INLINE zipWith5_ #-}

  default zipWith6_ :: Coercible (f a -> f b -> f c -> f d -> f e -> f g -> f x)
    (a -> b -> c -> d -> e -> g -> x)
    => (a -> b -> c -> d -> e -> g -> x) -> f a -> f b -> f c -> f d -> f e -> f g -> f x
  zipWith6_ = coerce
  {-# INLINE zipWith6_ #-}


-- * Zippable instances

instance Zippable Identity where
instance Zippable I where

-- * Applicativj

newtype ApplicativeZipper f a = ApplicativeZipper (f a)
  deriving (Functor, Applicative, Traversable, Foldable)

instance {-# OVERLAPS #-} Applicative f => Zippable (ApplicativeZipper f) where
  zipWith_ = liftA2
  {-# INLINE zipWith_ #-}
  zipWith3_ = liftA3
  {-# INLINE zipWith3_ #-}
  zipWith4_ = liftA4
  {-# INLINE zipWith4_ #-}
  zipWith5_ = liftA5
  {-# INLINE zipWith5_ #-}
  zipWith6_ = liftA6
  {-# INLINE zipWith6_ #-}

deriving via ApplicativeZipper ((,) a)          instance {-# OVERLAPS #-} Monoid a => Zippable ((,) a) 
deriving via ApplicativeZipper IO               instance {-# OVERLAPS #-} Zippable IO
deriving via ApplicativeZipper ((->) a)         instance {-# OVERLAPS #-} Zippable ((->) a) 
deriving via ApplicativeZipper Maybe            instance {-# OVERLAPS #-} Zippable Maybe
deriving via ApplicativeZipper (Const a)        instance {-# OVERLAPS #-} Monoid a => Zippable (Const a) 
deriving via ApplicativeZipper (Either e)       instance {-# OVERLAPS #-} Zippable (Either e)

-- * instances using existing zipping functions, where Applicative would yield the wrong result

instance {-# OVERLAPS #-} Zippable [] where
  zipWith_ = zipWith
  {-# INLINABLE zipWith_ #-}
  zipWith3_ = zipWith3
  {-# INLINE zipWith3_ #-}
  zipWith4_ = zipWith4
  {-# INLINE zipWith4_ #-}
  zipWith5_ = zipWith5
  {-# INLINE zipWith5_ #-}
  zipWith6_ = zipWith6
  {-# INLINE zipWith6_ #-}

instance {-# OVERLAPS #-} Zippable V.Vector where
  zipWith_ = V.zipWith
  {-# INLINABLE zipWith_ #-}
  zipWith3_ = V.zipWith3
  {-# INLINE zipWith3_ #-}
  zipWith4_ = V.zipWith4
  {-# INLINE zipWith4_ #-}
  zipWith5_ = V.zipWith5
  {-# INLINE zipWith5_ #-}
  zipWith6_ = V.zipWith6
  {-# INLINE zipWith6_ #-}

-- * isomorphic to 2 nested functors 

class ZippableHelper h f g | h -> f, h -> g where
  wrapAfterZip          :: f (g x) -> h x
  unwrapBeforeZip       :: h x -> f (g x)

instance {-# OVERLAPPABLE #-} (Functor h, Zippable f, Zippable g, ZippableHelper h f g) => Zippable h where
  zipWith_      = zipWithUnwrapped2 wrapAfterZip unwrapBeforeZip
  zipWith3_     = zipWithUnwrapped3 wrapAfterZip unwrapBeforeZip
  zipWith4_     = zipWithUnwrapped4 wrapAfterZip unwrapBeforeZip
  zipWith5_     = zipWithUnwrapped5 wrapAfterZip unwrapBeforeZip
  zipWith6_     = zipWithUnwrapped6 wrapAfterZip unwrapBeforeZip
  
  
zipWithUnwrapped2 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
  -> (forall x . h x -> f (g x)) -> (a -> b -> c) -> h a -> h b -> h c
zipWithUnwrapped2 wrap unwrap f a b = wrap $ zipWith_ (zipWith_ f) (unwrap a) $ unwrap b
{-# INLINE zipWithUnwrapped2 #-}

zipWithUnwrapped3 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
  -> (forall x . h x -> f (g x)) -> (a -> b -> c -> z) -> h a -> h b -> h c -> h z
zipWithUnwrapped3 wrap unwrap f a b c = wrap $ zipWith3_ (zipWith3_ f) (unwrap a) (unwrap b) (unwrap c)
{-# INLINE zipWithUnwrapped3 #-}

zipWithUnwrapped4 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
  -> (forall x . h x -> f (g x)) -> (a -> b -> c -> d -> z) -> h a -> h b -> h c -> h d -> h z
zipWithUnwrapped4 wrap unwrap f a b c d = wrap $ zipWith4_ (zipWith4_ f) (unwrap a) (unwrap b) (unwrap c)
  (unwrap d)
{-# INLINE zipWithUnwrapped4 #-}

zipWithUnwrapped5 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
  -> (forall x . h x -> f (g x)) -> (a -> b -> c -> d -> e -> z) -> h a -> h b -> h c -> h d -> h e -> h z
zipWithUnwrapped5 wrap unwrap f a b c d e = wrap $ zipWith5_ (zipWith5_ f) (unwrap a) (unwrap b) (unwrap c)
  (unwrap d) (unwrap e)
{-# INLINE zipWithUnwrapped5 #-}

zipWithUnwrapped6 :: (Zippable f, Zippable g) => (forall x . f (g x) -> h x)
  -> (forall x . h x -> f (g x)) -> (a -> b -> c -> d -> e -> y -> z)
  -> h a -> h b -> h c -> h d -> h e -> h y -> h z
zipWithUnwrapped6 wrap unwrap f a b c d e g = wrap $ zipWith6_ (zipWith6_ f) (unwrap a) (unwrap b) (unwrap c)
  (unwrap d) (unwrap e) (unwrap g)
{-# INLINE zipWithUnwrapped6 #-}

instance Zippable m => ZippableHelper (ExceptT e m) m (Either e) where
  wrapAfterZip = ExceptT
  {-# INLINE wrapAfterZip #-}
  unwrapBeforeZip = runExceptT
  {-# INLINE unwrapBeforeZip #-}

instance Zippable m => ZippableHelper (ReaderT r m) ((->) r) m where
  wrapAfterZip = ReaderT
  {-# INLINE wrapAfterZip #-}
  unwrapBeforeZip = runReaderT
  {-# INLINE unwrapBeforeZip #-}

instance (Zippable f, Zippable g) => ZippableHelper (f :.: g) f g where
  wrapAfterZip = Comp
  {-# INLINE wrapAfterZip #-}
  unwrapBeforeZip = unComp
  {-# INLINE unwrapBeforeZip #-}

instance (Monoid w, Zippable m) => ZippableHelper (WriterT w m) m ((,) w) where
  wrapAfterZip = WriterT . fmap swap
  {-# INLINE wrapAfterZip #-}
  unwrapBeforeZip = fmap swap . runWriterT
  {-# INLINE unwrapBeforeZip #-}


liftA4 :: Applicative f
  => (a1 -> a2 -> a3 -> a4 -> b) -> f a1 -> f a2 -> f a3 -> f a4 -> f b
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
{-# INLINABLE liftA4 #-}

liftA5 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> b)
       -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f b
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
{-# INLINABLE liftA5 #-}

liftA6 :: Applicative f => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
       -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f a6 -> f b
liftA6 f a b c d e g = f <$> a <*> b <*> c <*> d <*> e <*> g
{-# INLINABLE liftA6 #-}


zip_ :: forall a b f . Zippable f => f a -> f b -> f (a,b)
zip_ = zipWith_ (,)
{-# INLINE zip_ #-}

zip3_ ::forall a b c f .  Zippable f => f a -> f b -> f c -> f (a,b,c)
zip3_ = zipWith3_ (,,)
{-# INLINE zip3_ #-}

zip4_ :: forall a b c d f . Zippable f => f a -> f b -> f c -> f d -> f (a,b,c,d)
zip4_ = zipWith4_ (,,,)
{-# INLINE zip4_ #-}

zip5_ :: forall a b c d e f . Zippable f => f a -> f b -> f c -> f d -> f e -> f (a,b,c,d,e)
zip5_ = zipWith5_ (,,,,)
{-# INLINE zip5_ #-}

zip6_ :: forall a b c d e g f . Zippable f => f a -> f b -> f c -> f d -> f e -> f g -> f (a,b,c,d,e,g)
zip6_ = zipWith6_ (,,,,,)
{-# INLINE zip6_ #-}

class (Traversable f, Foldable f, Functor f) => Container f where
  containerUnzip              :: f (x,y) -> (f x, f y)
  containerZipWith3           :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  containerPartitionEithers   :: f (Either x y) -> (f x, f y)
  containerPartitionWith      :: (a -> Either x y) -> f a -> (f x, f y)
  containerFilter             :: (a -> Bool) -> f a -> f a 

instance Container Maybe where
  containerUnzip              = fmap fst &&& fmap snd
  containerPartitionEithers x = (x >>= leftToMaybe, x >>= rightToMaybe)
  containerFilter           f = (>>= \x -> x <$ guard (f x))
  containerPartitionWith    f = containerPartitionEithers . fmap f
  containerZipWith3           = liftA3
  {-# INLINE containerUnzip #-}
  {-# INLINE containerPartitionEithers #-}
  {-# INLINE containerFilter #-}
  {-# INLINE containerPartitionWith #-}

instance Container V.Vector where
  containerUnzip              = V.unzip
  containerPartitionEithers   = V.partitionWith id
  containerFilter             = V.filter
  containerPartitionWith      = V.partitionWith
  containerZipWith3           = V.zipWith3
  {-# INLINE containerUnzip #-}
  {-# INLINE containerPartitionEithers #-}
  {-# INLINE containerFilter #-}
  {-# INLINE containerPartitionWith #-}

instance Container [] where
  containerUnzip              = unzip
  containerPartitionEithers   = partitionEithers
  containerPartitionWith    f = partitionEithers . fmap f
  containerFilter             = filter
  containerZipWith3           = zipWith3
  {-# INLINE containerUnzip #-}
  {-# INLINE containerPartitionEithers #-}
  {-# INLINE containerFilter #-}
  {-# INLINE containerPartitionWith #-}

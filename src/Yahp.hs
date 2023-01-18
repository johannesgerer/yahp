{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Yahp
  (module Yahp
  ,module S
  ) where

-- import           Control.Monad.Except as S
-- import           Control.Monad.RWS as S hiding (pass)
-- import           Control.Monad.Reader as S

import           Control.Arrow as S
import           Control.Exception as S hiding (throwIO, throwTo)
import           Control.Lens
import           Control.Monad as S
import qualified Data.DList as D
import           Control.Monad.Writer (tell, runWriter, censor)
import           Control.Monad.Writer as S (MonadWriter, Writer)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Char as S
import           Data.Coerce
import           Data.Fixed as S
import qualified Data.Foldable as UnsafeFoldable
import           Data.Function as S
import           Data.Functor as S
import           Data.Bitraversable as S
import           Data.Graph
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML
import           Data.Int as S
import           Data.List as S
  ( lookup
  , zip3
  , zip4
  , zip5
  , zipWith3
  , zipWith4
  , zipWith5
  , unzip3
  , unzip4
  , unzip5
  , delete
  , partition)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import           Data.Maybe as S hiding (fromJust)
import           Data.Monoid ()
import           Data.Ord as S
import           Data.Primitive.Array (array#)
import           Data.SOP (I(..))
import           Data.SOP as S (I(..), unI, (:.:)(..), unComp)
import           Data.String as S (String, fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Typeable as S
import           Data.Type.Equality as S
import qualified Data.Vector as V
import           GHC.Arr
import           GHC.Stack as S (popCallStack, freezeCallStack, emptyCallStack) -- not exported by protolude
import           GHC.Exception (errorCallWithCallStackException)
import qualified GHC.Exts as G
import           GHC.Exts as S (IsList(Item), fromList)
import qualified GHC.Generics as G
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax
import qualified Prelude
import qualified Protolude as P
import Protolude as S hiding (
  -- | GHC.Denerics
  (:.:)
  -- | GHC.OverloadedLabels
  ,IsLabel(..)
  -- | Data.Text.Encoding.Error
  ,lenientDecode , ignore , strictDecode , replace
  -- | NE.head
  , head
  -- | import Control.Exception
  ,catch
  -- | https://github.com/sdiehl/protolude/issues/110
  , minimum, minimumBy, maximum, maximumBy
  -- | GHC.generics
  ,from, to
  -- | GHC.Types
  , Symbol
  -- | bifunctor
  ,first,second, show, map)
import           Protolude.Error as S
import           Relude.Extra.Tuple as S
import           System.IO.Error as S
import           System.Directory
import           System.IO.Unsafe
import           Text.Read as S (Read(..), read, readParen)
import           Text.Show as S
import           TextShow as S (showt)
import           Yahp.FinancialUnits as S (showFinancialUnits, showThousandsSeparatorUS
                                          ,showThousandsSeparator, roundAwayFrom0
                                          ,FinancialUnits(..), ThousandsSeparators(..))
import           Yahp.Zippable as S
import Control.Concurrent.Async as S
  (concurrently_
  ,mapConcurrently
  ,forConcurrently
  ,mapConcurrently_
  ,forConcurrently_
  ,replicateConcurrently
  ,replicateConcurrently_ 
  -- ,concurrentlyE 
  -- ,ConcurrentlyE(..)
  -- ,runConcurrentlyE 
  ,compareAsyncs)

-- {-# INLINE pshow #-}
-- pshow :: (Show a, StringConv String b) => a -> b
-- pshow = P.show

{-# INLINE minimum #-}
minimum :: Ord a => NonEmpty a -> a
minimum = UnsafeFoldable.minimum

{-# INLINE minimumBy #-}
minimumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
minimumBy = UnsafeFoldable.minimumBy

{-# INLINE maximum #-}
maximum :: Ord a => NonEmpty a -> a
maximum = UnsafeFoldable.maximum

{-# INLINE maximumBy #-}
maximumBy :: (a -> a -> Ordering) -> NonEmpty a -> a
maximumBy = UnsafeFoldable.maximumBy


{-# INLINE foldr1 #-}
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = UnsafeFoldable.foldr1

{-# INLINE foldl1' #-}
foldl1' :: (a -> a -> a) -> NonEmpty a -> a
foldl1' f (a :| b) = UnsafeFoldable.foldl' f a b

{-# INLINE foldl1 #-}
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 = UnsafeFoldable.foldl1
  
{-# INLINE headMaybe #-}
headMaybe :: Foldable f => f a -> Maybe a
headMaybe = P.head

{-# INLINE tail #-}
tail :: NonEmpty a -> [a]
tail = NE.tail

{-# INLINE head #-}
head :: NonEmpty a -> a
head = NE.head


{-# INLINE toList' #-}
toList' :: G.IsList l => l -> [G.Item l]
toList' = G.toList

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap2
{-# INLINE (<$$>) #-}

infixl 4 <$$>

fmap2 :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
fmap2 = fmap . fmap
{-# INLINE fmap2 #-}

fmap3 :: (Functor f1, Functor f2, Functor f3) => (a -> b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
fmap3 = fmap2 . fmap
{-# INLINE fmap3 #-}

fmap4 :: (Functor f1, Functor f2, Functor f3, Functor f4)
  => (a -> b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
fmap4 = fmap2 . fmap2
{-# INLINE fmap4 #-}

pure3 :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 a))
pure3 = pure . pure . pure
{-# INLINE pure3 #-}

pure2 :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
pure2 = pure . pure
{-# INLINE pure2 #-}

traverse3 :: (Applicative f, Traversable t1, Traversable t2,Traversable t3)
  => (a -> f b) -> t1 (t2 (t3 a)) -> f (t1 (t2 (t3 b)))
traverse3 = traverse . traverse . traverse
{-# INLINE traverse3 #-}

traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t1 (t2 a) -> f (t1 (t2 b))
traverse2 = traverse . traverse
{-# INLINE traverse2 #-}

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap
{-# INLINE ffor #-}

chain :: Monad m => (a -> m b) -> m a -> m b
chain = (=<<)
{-# INLINE chain #-}

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)
{-# INLINE bind #-}

chain2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
chain2 f a b = do x <- a; y <- b; f x y 
{-# INLINE chain2 #-}

both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x,y) = (f x, f y)
{-# INLINE both #-}

enumAll' :: (Bounded a, Enum a) => NonEmpty a
enumAll' = NE.fromList [minBound..]
{-# INLINE enumAll' #-}

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound..]
{-# INLINE enumAll #-}

maybeThrow :: MonadError e m => e -> Maybe a -> m a
maybeThrow msg = maybe (throwError msg) return
{-# INLINE maybeThrow #-}

eitherThrow :: MonadError e m => Either e a -> m a
eitherThrow = either throwError pure
{-# INLINABLE eitherThrow #-}

lookupThrow' :: (ConvertText Text e, MonadError e m, Ixed (t a)
                , Typeable b, Show b, FoldableWithIndex b t, Index (t a) ~ b) =>
                Maybe Int -> Text -> Index (t a) -> t a -> m (IxValue (t a))
lookupThrow' showKeys msg k c = maybeThrow (toS $ err <> msg) $ c ^? ix k
  where err =  shot k <> " (type: " <> shot (typeOf k) <> ") not found in container. " <> case showKeys of
          Nothing -> "Count " <> shot len
          Just n  -> let lim = bool "" (" (showing " <> shot n <> " of " <> shot len <> ")") $ n < len
            in "Available keys" <> lim <> ":\n" <> unlines2 (shot <$> take n (c ^.. ifolded . asIndex))
        len = length c 
{-# INLINEABLE lookupThrow' #-}

lookupThrow :: (ConvertText Text e, MonadError e m, Ixed (t a)
                , Typeable b, Show b, FoldableWithIndex b t, Index (t a) ~ b) =>
                Index (t a) -> t a -> m (IxValue (t a))
lookupThrow = lookupThrow' (Just maxBound) ("" :: Text)
{-# INLINE lookupThrow #-}

tellErrors :: (Foldable f, MonadWriter [a] m) => f (Either a b) -> m [b]
tellErrors = mapTellErrors id
{-# INLINE tellErrors #-}

tellError :: MonadWriter [a] m => Either a b -> m (Maybe b)
tellError = either (\x -> Nothing <$ tell [x]) pure2
{-# INLINABLE tellError #-}

prependMsg' :: (Functor f, Semigroup a, MonadWriter (f a) m) => a -> m b -> m b
prependMsg' msg = censor $ fmap (msg <>)
{-# INLINABLE prependMsg' #-}


mapTellErrors :: (Foldable f, MonadWriter [a] m) => (v -> Either a b) -> f v -> m [b]
mapTellErrors f xs = let (errs, vals) = partitionEithers $ fmap f $ toList xs in vals <$ tell errs
{-# INLINEABLE mapTellErrors #-}

vMapTellErrors :: (MonadWriter [a] m) => (v -> Either a b) -> V.Vector v -> m (V.Vector b)
vMapTellErrors = fmap2 (\(e,v) -> v <$ tell (toList e)) V.partitionWith
{-# INLINEABLE vMapTellErrors #-}

noErrors :: (IsString e, Monoid e, MonadError e m) => Writer [e] a -> m a
noErrors = (\(a, es) -> if null es then pure a else unlineErrors es) . runWriter
{-# INLINEABLE noErrors #-}

sequenceErrors :: (MonadError e f, Monoid e, IsString e, Foldable t) => t (Either e b) -> f [b]
sequenceErrors = mapErrors id
{-# INLINE sequenceErrors #-}

mapErrors :: (MonadError e f, Monoid e, IsString e, Foldable t) => (a -> Either e b) -> t a -> f [b]
mapErrors f xs = do
  let (errs, vals) = partitionEithers $ f <$> toList xs
  vals <$ when (not $ null errs) (unlineErrors errs)
{-# INLINEABLE mapErrors #-}

-- | only available in mtl 2.3.1
tryError2 :: MonadError a1 m => m a2 -> m (Either a1 a2)
tryError2 action = (Right <$> action) `catchError` (pure . Left)

liftErrors2 :: (MonadError e m, Monoid e, IsString e) => (t1 -> t2 -> b) -> m t1 -> m t2 -> m b
liftErrors2 f a b = do
  aE <- tryError2 a
  bE <- tryError2 b
  case (aE, bE) of
    (Left ae, Left be) -> unlineErrors [ae,be]
    (Left ae, _) -> throwError ae
    (_, Left be) -> throwError be
    (Right av, Right bv) -> pure $ f av bv
{-# INLINABLE liftErrors2 #-}

unlineErrors :: (MonadError e m, Monoid e, IsString e, Foldable t) => t e -> m a
unlineErrors = throwError . mconcat . intersperse (fromString "\n") . toList
{-# INLINEABLE unlineErrors #-}

modifyError :: MonadError e m => (e -> e) -> m a -> m a
modifyError f = flip catchError (throwError . f)
{-# INLINE modifyError #-}

eitherException :: ConvertText s String => Either s a -> a
eitherException = either (Prelude.error . toS) id
{-# INLINE eitherException #-}

appendCallStack :: (HasCallStack, MonadError e m, Semigroup e, ConvertText String e, IsString e) => m a -> m a
appendCallStack = modifyError $ \e -> e <> fromString "\n" <> toS (prettyCallStack $ popCallStack callStack)

catchWhnf :: (Exception ex, MonadError er m) => (ex -> er) -> a -> m a
catchWhnf f a = unsafePerformIO $ catch (seq a $ pure2 a) (pure . throwError . f)
{-# INLINEABLE catchWhnf #-}

forMaybeM :: Applicative m => c -> Maybe a -> (a -> m c) -> m c
forMaybeM = flip . maybeM
{-# INLINE forMaybeM #-}

forMaybe :: c -> Maybe a -> (a -> c) -> c
forMaybe = flip . maybe
{-# INLINE forMaybe #-}

maybeM :: Applicative m => c -> (a -> m c) -> Maybe a -> m c
maybeM x y = \case { Just a -> y a; Nothing -> pure x }
{-# INLINE maybeM #-}


fromUtf8 :: B.ByteString -> Text
fromUtf8 = decodeUtf8With T.strictDecode

fromUtf8Lenient :: B.ByteString -> Text
fromUtf8Lenient = decodeUtf8With T.lenientDecode

fromUtf8Lazy :: LB.ByteString -> LText
fromUtf8Lazy = TL.decodeUtf8With T.strictDecode

fromUtf8LazyLenient :: LB.ByteString -> LText
fromUtf8LazyLenient = TL.decodeUtf8With T.lenientDecode


-- fcheck :: IO ()
-- fcheck = let fl x y = trace ("trace: " <> [Prelude.head y]) $ concat ["(",x,"+",y,")"]
--              fr x y = trace ("trace: " <> [Prelude.head x]) $ concat ["(",x,"+",y,")"]
--              l = ["a","b","c","d","e"]
--   in putStrLn $ unlines $ fmap ((\x -> seq x (x::Text)) . toS. trace "")
--   [foldl   fl       "0"     l
--   ,foldl1  fl $     "0" :|  l
--   ,foldl'  fl       "0"     l
--   ,foldl1' fl $     "0" :|  l
--   ,foldr   fr       "0"     l
--   ,foldr'  fr       "0"     l
--   ,foldr1  fr $     "0" :|  l
--   ]

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- trace: e

-- trace: e
-- trace: d
-- trace: c
-- trace: b
-- trace: a

-- trace: 0
-- trace: a
-- trace: b
-- trace: c
-- trace: d
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (((((0+a)+b)+c)+d)+e)
-- (a+(b+(c+(d+(e+0)))))
-- (a+(b+(c+(d+(e+0)))))
-- (0+(a+(b+(c+(d+e)))))

class IsLabel (x :: P.Symbol) a where
  fromLabel :: HasCallStack => a

instance {-# OVERLAPPABLE #-} P.IsLabel m a => IsLabel m a where
  fromLabel = P.fromLabel @m
  
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ b = b
{-# INLINE ifThenElse #-}


hmKeys :: HM.HashMap k a -> [k]
hmKeys = HM.keys
{-# INLINE hmKeys #-}

mKeys :: M.Map k a -> [k]
mKeys = M.keys
{-# INLINE mKeys #-}

slap :: Functor f => a -> f b -> f a
slap = (<$)
{-# INLINE slap #-}

shot :: Show a => a -> Text
shot = toS . show
{-# INLINE shot #-}

makeLensesWithSuffix :: Name -> DecsQ
makeLensesWithSuffix = makeLensesWith (lensField .~ mappingNamer (\f -> [f <> "_"]) $ lensRules)


cycles :: Ord k => [(n,k,[k])] -> [[n]]
cycles = mapMaybe (\case CyclicSCC v -> Just v ; _ -> Nothing) . stronglyConnComp
{-# INLINABLE cycles #-}


coerce' :: forall b a . Coercible a b => a -> b
coerce' = coerce
{-# INLINE coerce' #-}

coerce1 :: forall a b f . Coercible (f a) (f b) => f a -> f b
coerce1 = coerce
{-# INLINE coerce1 #-}

coerce2 :: forall a b g f . Coercible (f (g a)) (f (g b)) => f (g a) -> f (g b)
coerce2 = coerce
{-# INLINE coerce2 #-}

coerce1' :: forall b a f . Coercible (f a) (f b) => f a -> f b
coerce1' = coerce
{-# INLINE coerce1' #-}

coerce2' :: forall b a g f . Coercible (f (g a)) (f (g b)) => f (g a) -> f (g b)
coerce2' = coerce
{-# INLINE coerce2' #-}

-- | WITHOUT trailing newline
unlinesT :: [Text] -> Text
unlinesT = T.intercalate $ T.singleton '\n'
{-# INLINE unlinesT #-}

-- * EitherC

class EitherC f a b where
  eitherC1 :: (g a -> x) -> (g b -> x) -> g f -> x

eitherC1_ :: forall g a b x f . EitherC f a b => (g a -> x) -> (g b -> x) -> g f -> x
eitherC1_ = eitherC1
{-# INLINE eitherC1_ #-}

eitherC0 :: forall a b x f . EitherC f a b => (a -> x) -> (b -> x) -> f -> x
eitherC0 = coerce (eitherC1_ @I @a @b @x @f)
{-# INLINE eitherC0 #-}

eitherC2 :: forall a b h g x f . EitherC f a b => (h (g a) -> x) -> (h (g b) -> x) -> h (g f) -> x
eitherC2 = coerce (eitherC1_ @(h :.: g) @a @b @x @f)
{-# INLINE eitherC2 #-}

eitherCS :: forall a b g x f . EitherC f a b => (a g -> x) -> (b g -> x) -> f g -> x
eitherCS = coerce (eitherC1_ @(SwapT g) @a @b @x @f)
{-# INLINE eitherCS #-}

eitherC1S :: forall a b h g x f . EitherC f a b => (h (a g) -> x) -> (h (b g) -> x) -> h (f g) -> x
eitherC1S = coerce (eitherC1_ @(Swap1T h g) @a @b @x @f)
{-# INLINE eitherC1S #-}

eitherC2S :: forall a b h g t x f . EitherC f a b => (t (h (a g)) -> x) -> (t (h (b g)) -> x) -> t (h (f g)) -> x
eitherC2S = coerce (eitherC1_ @(Swap2T t h g) @a @b @x @f)
{-# INLINE eitherC2S #-}

newtype SwapT a b = SwapT (b a)
newtype Swap1T a b c = Swap1T (a (c b))
newtype Swap2T t a b c = Swap2T (t (a (c b)))

instance EitherC a a b where eitherC1 g _ = g
                             {-# INLINE eitherC1 #-}

instance EitherC b a b where eitherC1 _ g = g
                             {-# INLINE eitherC1 #-}


vectorArray :: (HasCallStack, Ix i) => (i, i) -> V.Vector e -> Array i e
vectorArray (l,u) vec | rs > length vec = G.raise# $ errorCallWithCallStackException msg callStack 
                      | True = GHC.Arr.Array l u rs (array# (V.toArray vec))
  where rs = rangeSize (l,u)
        msg = "length mismatch: rangeSize " <> show rs <> " > vector length " <> show (length vec)
{-# INLINABLE vectorArray #-}

class IsBool a where isTrue :: a -> Bool
instance IsBool Bool where isTrue = id
                           {-# INLINE isTrue #-}


capitalizeFirst :: Text -> Text
capitalizeFirst = maybe mempty (\(h,t) -> T.cons (toUpper h) t) . T.uncons

-- * Single Constructor


unwrapSingleConstructor :: (Generic a, Rep a ~ D1 d (C1 c (S1 s (Rec0 v)))) => a -> v
unwrapSingleConstructor x = unK1 $ unM1 $ unM1 $ unM1 $ G.from x
{-# INLINEABLE unwrapSingleConstructor #-}


rewrapSingleConstructor :: (Generic a, Rep a ~ D1 d (C1 c (S1 s (Rec0 v)))) => v -> a
rewrapSingleConstructor   = G.to . M1 . M1 . M1 . K1
{-# INLINEABLE rewrapSingleConstructor #-}

traceBy :: Print b => (a -> b) -> a -> a
traceBy f x = trace (f x) x
{-# WARNING traceBy "'traceBy' remains in code" #-}

traceShowBy :: Show a => (b -> a) -> b -> b
traceShowBy f x = traceShow (f x) x
{-# WARNING traceShowBy "'traceShowBy' remains in code" #-}

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = catchIOError (removeFile f) $ \e -> if isDoesNotExistError e then pass
                                                           else ioError e

groupMapWith :: Ord a => (b -> a) -> [b] -> ML.Map a [b]
groupMapWith f = fmap D.toList . ML.fromListWith (<>) . fmap (f &&& D.singleton)
{-# INLINABLE groupMapWith #-}

groupHashMapWith :: (Eq a, Hashable a) => (b -> a) -> [b] -> HML.HashMap a [b]
groupHashMapWith f = fmap D.toList . HML.fromListWith (<>) . fmap (f &&& D.singleton)
{-# INLINABLE groupHashMapWith #-}

groupMap :: Ord a => [(a,b)] -> ML.Map a [b]
groupMap = fmap D.toList . ML.fromListWith (<>) . fmap2 D.singleton
{-# INLINABLE groupMap #-}

groupHashMap :: (Eq a, Hashable a) => [(a,b)] -> HML.HashMap a [b]
groupHashMap = fmap D.toList . HML.fromListWith (<>) . fmap2 D.singleton
{-# INLINABLE groupHashMap #-}

-- | this is defined here to get rid of the deprecation warning in text-1.2.5.0
decodeASCIILazy :: LByteString -> LText
decodeASCIILazy = TL.decodeASCII

-- | this is defined here to get rid of the deprecation warning in text-1.2.5.0
decodeASCII :: ByteString -> Text
decodeASCII = T.decodeASCII

-- | jesus
unlines2 :: [Text] -> Text
unlines2 = T.intercalate $ T.singleton '\n'

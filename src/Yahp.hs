{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Text.InterpolatedString.Perl6
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
import           Data.Default as S
import           Data.These as S
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
import qualified Data.Set as Set
import qualified Data.Map.Lazy as ML
import           Data.Maybe as S hiding (fromJust)
import           Data.Monoid ()
import           Data.Ord as S
import           Data.Primitive.Array (array#)
import           Data.SOP (I(..))
import           Data.SOP as S (I(..), unI, (:.:)(..), unComp)
import           Data.String as S (String, fromString)
-- import qualified Data.String as P
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
  -- | use GHC.OverloadedLabels + Rebindable syntax to get Yahp.fromLabel/fromString with CallStack
  -- (starting from GHC on or before 9.4.8)
  ,IsLabel(..) -- , IsString (replacing this is too annoying for too little benefit)
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
  ,first,second, show, map
  ,readEither
  )
import           Protolude.Error as S
import           Relude.Extra.Tuple as S
import           System.IO.Error as S
import           System.Directory
import           System.IO.Unsafe
import           Text.Read as S (Read(..), read, readParen)
import           qualified Text.Read
import           Text.Show as S
import           TextShow as S (showt)
import           Yahp.FinancialUnits as S (showFinancialUnits, showThousandsSeparatorUS
                                          ,showThousandsSeparator, roundAwayFrom0
                                          ,FinancialUnits(..), ThousandsSeparators(..)
                                          ,roundToMultiplesOf)
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

type NE = NonEmpty

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

pure4 :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => a -> f1 (f2 (f3 (f4 a)))
pure4 = pure3 . pure
{-# INLINE pure4 #-}

pure3 :: (Applicative f1, Applicative f2, Applicative f3) => a -> f1 (f2 (f3 a))
pure3 = pure2 . pure
{-# INLINE pure3 #-}

pure2 :: (Applicative f1, Applicative f2) => a -> f1 (f2 a)
pure2 = pure . pure
{-# INLINE pure2 #-}

pass4 :: (Applicative f1, Applicative f2, Applicative f3, Applicative f4) => f1 (f2 (f3 (f4 ())))
pass4 = pure4 ()
{-# INLINE pass4 #-}

pass3 :: (Applicative f1, Applicative f2, Applicative f3) => f1 (f2 (f3 ()))
pass3 = pure3 ()
{-# INLINE pass3 #-}

pass2 :: (Applicative f1, Applicative f2) => f1 (f2 ())
pass2 = pure2 ()
{-# INLINE pass2 #-}

traverse4 :: (Applicative f, Traversable t1, Traversable t2, Traversable t3, Traversable t4)
  => (a -> f b) -> t1 (t2 (t3 (t4 a))) -> f (t1 (t2 (t3 (t4 b))))
traverse4 = traverse . traverse . traverse . traverse
{-# INLINE traverse4 #-}

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

-- λ> either (putStrLn @Text) print $ lookupThrow' 2 "asd" 5 $ M.fromList $ zip [1,2 :: Int,3,4] "hall"
-- Available keys (showing 2 of 4):
-- 1
-- 2
-- 5 (type: Int) not found in container (count: 4)"asd
lookupThrow' :: (ConvertText Text e, MonadError e m, Ixed (t a)
                , Typeable b, Show b, FoldableWithIndex b t, Index (t a) ~ b) =>
                Maybe Int -- ^ nothing: do not show count
                          --   Just 0: show no keys
                          --   Just maxBound: show all Keys
             -> Text -> Index (t a) -> t a -> m (IxValue (t a))
lookupThrow' showKeys msg k c = maybeThrow (toS $ err <> msg) $ c ^? ix k
  where err = keys [qq|{k} (type: {typeOf k}) not found in container|]
        keys x = case showKeys of
          Nothing       -> x
          Just 0        -> x <>[qq| (count: {len})|]
          Just n        -> let lim = bool "" [qq| (showing {n} of {len})|] $ n < len
                           in "Available keys" <> lim <> ":\n"
                                <> T.unlines (shot <$> take n (c ^.. ifolded . asIndex)) <> x
        len = length c 
{-# INLINEABLE lookupThrow' #-}

lookupThrow :: (ConvertText Text e, MonadError e m, Ixed (t a)
                , Typeable b, Show b, FoldableWithIndex b t, Index (t a) ~ b) =>
                Index (t a) -> t a -> m (IxValue (t a))
lookupThrow = lookupThrow' (Just maxBound) ("" :: Text)
{-# INLINE lookupThrow #-}

tell1 :: (Applicative f, MonadWriter (f a) m) => a -> m ()
tell1 = tell . pure
{-# INLINE tell1 #-}

tellErrors :: (Container f, MonadWriter [a] m) => f (Either a b) -> m (f b)
tellErrors = mapTellErrors id
{-# INLINE tellErrors #-}

tellError :: MonadWriter [a] m => Either a b -> m (Maybe b)
tellError = either (\x -> Nothing <$ tell [x]) pure2
{-# INLINABLE tellError #-}

prependMsg' :: (Functor f, Semigroup a, MonadWriter (f a) m) => a -> m b -> m b
prependMsg' msg = censor $ fmap (msg <>)
{-# INLINABLE prependMsg' #-}


mapTellErrors :: (Container f, MonadWriter [a] m) => (v -> Either a b) -> f v -> m (f b)
mapTellErrors f = containerPartitionWith f >>> \(errs, vals) -> vals <$ tell (toList errs)
{-# INLINEABLE mapTellErrors #-}

noErrors :: (IsString e, Monoid e, MonadError e m) => Writer [e] a -> m a
noErrors = (\(a, es) -> if null es then pure a else unlineErrors es) . runWriter
{-# INLINEABLE noErrors #-}

sequenceErrors :: (MonadError e m, Monoid e, IsString e, Container f) => f (Either e b) -> m (f b)
sequenceErrors = mapErrors id
{-# INLINE sequenceErrors #-}

mapErrors :: (MonadError e m, Monoid e, IsString e, Container f) => (a -> Either e b) -> f a -> m (f b)
mapErrors f xs = do
  let (errs, vals) = containerPartitionWith f xs
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

-- | use GHC.OverloadedLabels + Rebindable syntax to get Yahp.fromLabel with CallStack (starting from
-- GHC on or before 9.4.8)
class IsLabel (x :: P.Symbol) a where
  fromLabel :: HasCallStack => a

instance {-# OVERLAPPABLE #-} P.IsLabel m a => IsLabel m a where
  fromLabel = P.fromLabel @m
  
-- replacing this is too annoying for too little benefit:

-- -- | use GHC.OverloadedString + Rebindable syntax to get Yahp.fromString with CallStack (starting from
-- -- GHC on or before 9.4.8)
-- class IsString a where
--     fromString :: HasCallStack => String -> a

-- instance {-# OVERLAPPABLE #-} P.IsString a => IsString a where
--   fromString = P.fromString
  
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
unlinesT :: Foldable f => f Text -> Text
unlinesT = intercalateT $ T.singleton '\n'
{-# INLINE unlinesT #-}

intercalateT :: Foldable f => Text -> f Text -> Text
intercalateT s = T.intercalate s . toList
{-# INLINE intercalateT #-}

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

isFalse :: IsBool a => a -> Bool
isFalse = not . isTrue
{-# INLINE isFalse #-}


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
{-# INLINE traceBy #-}
{-# WARNING traceBy "'traceBy' remains in code" #-}

traceShowBy :: Show a => (b -> a) -> b -> b
traceShowBy f x = traceShow (f x) x
{-# INLINE traceShowBy #-}
{-# WARNING traceShowBy "'traceShowBy' remains in code" #-}

traceNoWarn :: Print b => b -> a -> a
traceNoWarn = trace
{-# INLINE traceNoWarn #-}

traceByNoWarn :: Print b => (a -> b) -> a -> a
traceByNoWarn = traceBy
{-# INLINE traceByNoWarn #-}

traceShowByNoWarn :: Show a => (b -> a) -> b -> b
traceShowByNoWarn = traceShowBy
{-# INLINE traceShowByNoWarn #-}

traceShowNoWarn :: Show a => a -> b -> b
traceShowNoWarn = traceShow
{-# INLINE traceShowNoWarn #-}

traceShowIdNoWarn :: Show a => a -> a
traceShowIdNoWarn = traceShowId
{-# INLINE traceShowIdNoWarn #-}

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = catchIOError (removeFile f) $ \e -> if isDoesNotExistError e then pass
                                                           else ioError e
-- * grouping

groupByMapNe :: (Foldable f, Ord a) => (b -> a) -> f b -> ML.Map a (NE b)
groupByMapNe = fmap3 NE.fromList groupByMap
{-# INLINE groupByMapNe #-}

groupByHashMapNe :: (Foldable f, Eq a, Hashable a) => (b -> a) -> f b -> HML.HashMap a (NE b)
groupByHashMapNe = fmap3 NE.fromList groupByHashMap
{-# INLINE groupByHashMapNe #-}

groupMapNe :: (Foldable f, Ord a) => f (a,b) -> ML.Map a (NE b)
groupMapNe = fmap2 NE.fromList groupMap
{-# INLINE groupMapNe #-}

groupHashMapNe :: (Eq a, Foldable f, Hashable a) => f (a,b) -> HML.HashMap a (NE b)
groupHashMapNe = fmap2 NE.fromList groupHashMap
{-# INLINE groupHashMapNe #-}

groupByMap :: (Foldable f, Ord a) => (b -> a) -> f b -> ML.Map a [b]
groupByMap f = fmap D.toList . ML.fromListWith (<>) . fmap (f &&& D.singleton) . toList
{-# INLINABLE groupByMap #-}

groupByHashMap :: (Foldable f, Eq a, Hashable a) => (b -> a) -> f b -> HML.HashMap a [b]
groupByHashMap f = fmap D.toList . HML.fromListWith (<>) . fmap (f &&& D.singleton) . toList
{-# INLINABLE groupByHashMap #-}

groupMap :: (Foldable f, Ord a) => f (a,b) -> ML.Map a [b]
groupMap = fmap D.toList . ML.fromListWith (<>) . fmap2 D.singleton . toList
{-# INLINABLE groupMap #-}

groupHashMap :: (Eq a, Foldable f, Hashable a) => f (a,b) -> HML.HashMap a [b]
groupHashMap = fmap D.toList . HML.fromListWith (<>) . fmap2 D.singleton . toList
{-# INLINABLE groupHashMap #-}

-- * duplicates

hmFromListTellDups :: forall a t v m. (MonadWriter [Text] m, Typeable a, Hashable a, Show a, Foldable t)
  => t (a,v) -> m (HM.HashMap a v)
hmFromListTellDups xs = HM.traverseWithKey g $ (groupHashMapNe xs :: HM.HashMap a (NonEmpty v))
  where g _ (x:|[])     = pure x
        g k (x:|r)      = x <$ tell ["Key (" <> showt (typeOf k) <> ") " <> shot k <> " appears "
                                     <> showt (succ $ length r) <> " times" :: Text]
        g :: a -> NonEmpty v -> m v
{-# INLINABLE hmFromListTellDups #-}

hmFromListByTellDups :: forall a t v m.
  (MonadWriter [Text] m, Functor t, Typeable a, Hashable a, Show a, Foldable t)
  => (v -> a) -> t v -> m (HM.HashMap a v)
hmFromListByTellDups f = hmFromListTellDups . fmapToFst f
{-# INLINABLE hmFromListByTellDups #-}

mFromListTellDups :: forall a t v m. (MonadWriter [Text] m, Typeable a, Ord a, Show a, Foldable t)
  => t (a,v) -> m (M.Map a v)
mFromListTellDups xs = M.traverseWithKey g $ (groupMapNe xs :: M.Map a (NonEmpty v))
  where g _ (x:|[])     = pure x
        g k (x:|r)      = x <$ tell ["Key (" <> showt (typeOf k) <> ") " <> shot k <> " appears "
                                     <> showt (succ $ length r) <> " times" :: Text]
        g :: a -> NonEmpty v -> m v
{-# INLINABLE mFromListTellDups #-}

mFromListByTellDups :: forall a t v m. (MonadWriter [Text] m, Functor t, Typeable a, Ord a, Show a, Foldable t)
  => (v -> a) -> t v -> m (M.Map a v)
mFromListByTellDups f = mFromListTellDups . fmapToFst f
{-# INLINABLE mFromListByTellDups #-}

readEither :: forall b t a . (ConvertText String t, ConvertText a String, Typeable b, Read b)
  => a -> Either t b
readEither (toS -> t) = left (const $ toS $ "Cannot parse '" <> t <> "' as " <> show (typeRep (Proxy @b)))
  $ Text.Read.readEither t

-- | λ> either putStrLn print =<< runM (runError $ checkAbsoluteError "error:\n" 0.01 100 102)
--   error:
--   abs(102.0 - 100.0)
--    = 2.0 > 1.0e-2
checkAbsoluteError :: (MonadError Text m, Ord a, Num a, Show a) => Text -> a -> a -> a -> m ()
checkAbsoluteError msg tol expected actual = when (dif > tol) $ throwError $ msg <>
  [qq|abs({actual} - {expected})
= {dif} > {tol}|]
  where dif = abs $ actual - expected
{-# INLINABLE checkAbsoluteError #-}


-- | λ> either putStrLn print =<< runM (runError $ checkRelativeError "error:\n" 0.01 100 102)
--   error:
--   abs(102.0 - 100.0)
--    = 2.0 > 1.01
--    = 1.0e-2 * 101.0
checkRelativeError :: (MonadError Text m, Fractional a, Ord a, Show a) => Text -> a -> a -> a -> m ()
checkRelativeError msg tol expected actual = when (dif > tol * avg) $ throwError $ msg <>
  [qq|abs({actual} - {expected})
= {dif} > {tol * avg}
= {tol} * 0.5 * abs({actual} + {expected})|]
  where dif = abs $ actual - expected
        avg = (abs actual + abs expected) * 0.5
{-# INLINABLE checkRelativeError #-}

partitionWith :: (c -> Either a b) -> [c] -> ([a], [b])
partitionWith f = partitionEithers . fmap f
{-# INLINABLE partitionWith #-}

fromTheseMaybe :: These a b -> (Maybe a, Maybe b)
fromTheseMaybe = fromTheseDef . bimap Just Just
{-# INLINE fromTheseMaybe #-}

fromTheseDef :: (Default a, Default b) => These a b -> (a, b)
fromTheseDef = fromThese def def
{-# INLINE fromTheseDef #-}

theseDef :: (Default a, Default b) => (a -> b -> c) -> These a b -> c
theseDef f = these (flip f def) (f def) f
{-# INLINE theseDef #-}

theseMaybe :: (Maybe a -> Maybe c1 -> c2) -> These a c1 -> c2
theseMaybe f = theseDef f . bimap Just Just
{-# INLINE theseMaybe #-}


stripSuffix' :: Text -> Text -> Text
stripSuffix' s t = fromMaybe t $ T.stripSuffix s t

stripPrefix' :: Text -> Text -> Text
stripPrefix' s t = fromMaybe t $ T.stripPrefix s t

-- | via `Set a`
sortUniqueNe :: Ord a => NE a -> NE a
sortUniqueNe = NE.fromList . toList . Set.fromList . toList
{-# INLINE sortUniqueNe #-}

sortByUniqueNe :: Ord a => (a -> a -> Ordering) -> NE a -> NE a
sortByUniqueNe f = NE.fromList . foldr g [] . sortBy f . toList
  where g x = \case {[] -> x:[] ; l@(h:_) -> if h==x then l else x:l}
{-# INLINE sortByUniqueNe #-}

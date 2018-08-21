{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
#if __GLASGOW_HASKELL__ < 710
-- I guess GHC decides better these days.
{-# LANGUAGE UndecidableInstances       #-}
#endif

{- |
A version of the 'Newtype' typeclass and related functions.
Primarily pulled from Conor McBride's Epigram work. Some examples:

>>> ala Sum foldMap [1,2,3,4]
10

>>> ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3
8

>>> under2 Min (<>) 2 1
1

>>> over All not (All False)
All {getAll = True)

There are neat things you can do with this with /any/ 'Generic' newtype.

 > {-# LANGUAGE DeriveGeneric #-}
 > import GHC.Generics
 >
 > (...)
 > newtype Example = Example Int
 >   deriving (Generic)
 >
 > instance Newtype Example
 >

The version of the 'Newtype' class exported by this module has only
one instance, which cannot be overridden. It has instances for
*all* and *only* newtypes with 'Generic' instances whose generated
'Coercible' instances are visible. This makes it more convenient than the
version in "Control.Newtype.Generics" when it applies.

Like McBride's version, and unlike the one in "Control.Newtype.Generics",
this version has two parameters: one for the newtype and one for the
underlying type. This generally makes 'Newtype' constraints more compact
and easier to read.

Note: this module is only available when compiling with GHC 7.8 and later.

@since TODO
-}
module Control.Newtype.Generics.Generic
  ( Newtype
  , O
  , op
  , ala
  , ala'
  , under
  , over
  , under2
  , over2
  , underF
  , overF
  ) where

import GHC.Generics
import Data.Coerce

-- | Get the underlying type of a newtype.
--
-- @
-- data N = N Int deriving Generic
-- -- O N = Int
-- @
type O x = GO (Rep x)

type family GO rep where
  GO (D1 d (C1 c (S1 s (K1 i a)))) = a

-- | @Newtype n o@ means that @n@ is a newtype wrapper around
-- @o@. @n@ must be an instance of 'Generic'. Furthermore, the
-- @'Coercible' n o@ instance must be visible; this typically
-- means the newtype constructor is visible, but the instance
-- could also have been brought into view by pattern matching
-- on a 'Data.Type.Coercion.Coercion'.
class (Coercible n o, O n ~ o) => Newtype n o

-- The Generic n constraint gives a much better type error if
-- n is not an instance of Generic. Without that, there's
-- just a mysterious message involving GO and Rep. With it, the
-- lousy error message still shows up, but at least there's
-- also a good one.
instance (Generic n, Coercible n o, O n ~ o) => Newtype n o

-- Compat shim for GHC 7.8
coerce' :: Coercible a b => b -> a
#if __GLASGOW_HASKELL__ >= 710
coerce' = coerce
#else
coerce' = coerce (\x -> x :: a) :: forall a b. Coercible a b => b -> a
#endif

-- | Wrap a value with a newtype constructor.
pack :: Newtype n o => o -> n
pack = coerce'

-- | Unwrap a newtype constructor from a value.
unpack :: Newtype n o => n -> o
unpack = coerce

-- |
-- This function serves two purposes:
--
-- 1. Giving you the unpack of a newtype without you needing to remember the name.
--
-- 2. Showing that the first parameter is /completely ignored/ on the value level,
--    meaning the only reason you pass in the constructor is to provide type
--    information.  Typeclasses sure are neat.
--
-- >>> op Identity (Identity 3)
-- 3
op :: Newtype n o => (o -> n) -> n -> o
op _ = unpack

-- | The workhorse of the package. Given a "packer" and a \"higher order function\" (/hof/),
-- it handles the packing and unpacking, and just sends you back a regular old
-- function, with the type varying based on the /hof/ you passed.
--
-- The reason for the signature of the /hof/ is due to 'ala' not caring about structure.
-- To illustrate why this is important, consider this alternative implementation of 'under2':
--
-- > under2 :: (Newtype n o, Newtype n' o')
-- >        => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
-- > under2' pa f o0 o1 = ala pa (\p -> uncurry f . bimap p p) (o0, o1)
--
-- Being handed the "packer", the /hof/ may apply it in any structure of its choosing –
-- in this case a tuple.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
ala :: (Newtype n o, Newtype n' o')
    => (o -> n) -> ((o -> n) -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work.
-- The way it differs from the 'ala' function in this package,
-- is that it provides an extra hook into the \"packer\" passed to the hof.
-- However, this normally ends up being @id@, so 'ala' wraps this function and
-- passes @id@ as the final parameter by default.
-- If you want the convenience of being able to hook right into the hof,
-- you may use this function.
--
-- >>> ala' Sum foldMap length ["hello", "world"]
-- 10
--
-- >>> ala' First foldMap (readMaybe @Int) ["x", "42", "1"]
-- Just 42
ala' :: (Newtype n o, Newtype n' o')
     => (o -> n) -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
--ala' _ hof f = unpack . hof (pack . f)
ala' _ = coerce

-- | A very simple operation involving running the function \'under\' the newtype.
--
-- >>> under Product (stimes 3) 3
-- 27
under :: (Newtype n o, Newtype n' o')
      => (o -> n) -> (n -> n') -> (o -> o')
--under _ f = unpack . f . pack
under _ = coerce

-- | The opposite of 'under'. I.e., take a function which works on the
-- underlying types, and switch it to a function that works on the newtypes.
--
-- >>> over All not (All False)
-- All {getAll = True}
over :: (Newtype n o,  Newtype n' o')
     => (o -> n) -> (o -> o') -> (n -> n')
--over _ f = pack . f . unpack
over _ = coerce'

-- | Lower a binary function to operate on the underlying values.
--
-- >>> under2 Any (<>) True False
-- True
--
-- @since 0.5.2
under2 :: (Newtype n o, Newtype n' o')
       => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
--under2 _ f o0 o1 = unpack $ f (pack o0) (pack o1)
under2 _ = coerce

-- | The opposite of 'under2'.
--
-- @since 0.5.2
over2 :: (Newtype n o, Newtype n' o')
       => (o -> n) -> (o -> o -> o') -> (n -> n -> n')
--over2 _ f n0 n1 = pack $ f (unpack n0) (unpack n1)
over2 _ = coerce'

-- | 'under' lifted into a Functor.
underF :: (Newtype n o, Newtype n' o', Functor f, Functor g)
       => (o -> n) -> (f n -> g n') -> (f o -> g o')
underF _ f = fmap unpack . f . fmap pack

-- | 'over' lifted into a Functor.
overF :: (Newtype n o, Newtype n' o', Functor f, Functor g)
      => (o -> n) -> (f o -> g o') -> (f n -> g n')
overF _ f = fmap pack . f . fmap unpack

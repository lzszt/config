{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Config.HKD
  ( Default,
    Partial,
    Complete,
    dynamic,
    genericApply,
    HKD,
  )
where

import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic (..), K1 (K1), M1 (M1), type (:*:) (..))

-- | The HKD (higher-kinded data) pattern.
type HKD :: (Type -> Type) -> Type -> Type
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a

-- | Abstract notion of applying some sort of "partial value" to a
-- "total value" of the same higher-kinded type.
type GApply :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Constraint
class GApply i idelta o where
  gapply :: i p -> idelta p -> o p

instance
  (GApply i idelta o, GApply i' idelta' o') =>
  GApply (i :*: i') (idelta :*: idelta') (o :*: o')
  where
  gapply (l :*: r) (ldelta :*: rdelta) =
    gapply l ldelta :*: gapply r rdelta
  {-# INLINE gapply #-}

instance
  GApply i idelta o =>
  GApply (M1 _a _b i) (M1 _adelta _bdelta idelta) (M1 _a' _b' o)
  where
  gapply (M1 x) (M1 d) = M1 $ gapply x d
  {-# INLINE gapply #-}

instance GApply (K1 a k) (K1 a (Maybe k)) (K1 a k) where
  gapply (K1 k) (K1 kdelta) = K1 $ fromMaybe k kdelta
  {-# INLINE gapply #-}

instance
  GApply
    (K1 a (Proxy k))
    (K1 a k)
    (K1 a k)
  where
  gapply (K1 _kProxy) (K1 k) = K1 k
  {-# INLINE gapply #-}

-- | Apply a delta (that is, a value parameterized with 'Maybe' and 'Identity') to a
--  "total" value (one that is parameterized only with 'Identity'). You can
--  use this if you defined a higher-kinded data type and derive a
--  'Generic' representation.
genericApply ::
  ( Generic (Default f),
    Generic (Partial f),
    Generic (Complete f),
    GApply (Rep (Default f)) (Rep (Partial f)) (Rep (Complete f))
  ) =>
  Default f ->
  Partial f ->
  Complete f
genericApply x delta =
  to (gapply (from x) (from delta))
{-# INLINE genericApply #-}

-- | Shorthand for defining default configs
type Default c = c Identity Proxy

-- | Shorthand for defining partial configs
type Partial c = c Maybe Identity

-- | Shorthand for defining complete config
type Complete c = c Identity Identity

-- | can be used to define dynamic values in a default config definition
dynamic :: Proxy a
dynamic = Proxy

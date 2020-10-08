{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif
module T917 where

import Control.Lens
#if __GLASGOW_HASKELL__ >= 800 && __GLASGOW_HASKELL__ < 806
import Data.Kind
#endif

-- Like Data.Functor.Const, but redfined to ensure that it is poly-kinded
-- across all versions of GHC, not just 8.0+
newtype Constant a (b :: k) = Constant a

data T917OneA (a :: k -> *) (b :: k -> *) = MkT917OneA
data T917OneB a b = MkT917OneB (T917OneA a (Const b))
$(makePrisms ''T917OneB)

data T917TwoA (a :: k -> *) (b :: k -> *) = MkT917TwoA
data T917TwoB a b = MkT917TwoB (T917TwoA a (Const b))
$(makeClassyPrisms ''T917TwoB)

#if __GLASGOW_HASKELL__ >= 800
data T917Three (a :: k) where
  MkT917Three :: T917Three (a :: *)
$(makePrisms ''T917Three)

data T917Four (a :: k) where
  MkT917Four :: T917Four (a :: *)
$(makePrisms ''T917Four)
#endif

module Test where

import Prelude

import Data.Tuple (Tuple(..))

zipProduct :: forall a b f. (Apply f) => f a -> f b -> f (Tuple a b)
zipProduct xs ys = Tuple <$> xs <*> ys

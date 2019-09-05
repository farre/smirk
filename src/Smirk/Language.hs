{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverlappingInstances #-}
{-# Language UndecidableInstances #-}
module Smirk.Language where

import Data.Accumulator hiding (singleton)
import Data.Function.Polyvariadic hiding (Apply, apply)

import GHC.TypeLits

import Smirk.Compiler
import Smirk.Core
import Smirk.Syntax

unit :: Value -> Grin Value
unit = grin . Unit

store :: Value -> Grin Value
store = grin . Store

fetch :: Value -> Integer -> Grin Value
fetch ref offset = bind ref (\var -> grin (Fetch var offset))

update :: Value -> Value -> Grin Value
update ref value  = bind ref (\var -> grin (Update var value))

define :: Definition f (Grin ()) => String -> f -> Grin ()
define name prog = (definition (someSymbolVal name) id prog) :: Grin ()

newtype Cases = Cases { uncase :: Grin ([Binding] -> [Binding]) }

instance Pattern a => Accumulator Cases (a -> Grin Value) where
  accumulate x (Cases alts) = Cases $ do
    alternative x >>= \case
      Nothing -> alts
      Just alt -> fmap (. (alt:)) alts

class Polyvariadic Cases (Grin Value) x => Match x where
  match :: Value -> x

instance Polyvariadic Cases (Grin Value) x => Match x where
  match v = polyvariadic (Cases (return id)) ((cases v) . fmap (flip ($) []) . uncase)

instance (Accumulator Cases m, Match x) => Match (m -> x) where
  match v = polyvariadic (Cases (return id)) ((cases v) . fmap (flip ($) []) . uncase)

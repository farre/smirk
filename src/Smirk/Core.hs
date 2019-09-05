{-# Language LambdaCase #-}
{-# Language GADTs #-}
module Smirk.Core where

import Control.Monad.Operational
import Control.Monad.State

import Smirk.Context
import Smirk.Syntax

data Typed a where
  Typed :: Pattern a => Expression -> Typed a

data Ignore = I

type Grin = ProgramT Typed GrinState
type GrinView = ProgramViewT Typed GrinState

bind :: Pattern a => a -> (Variable -> Grin Value) -> Grin Value
bind a f = do
  toValue a >>= \case
    Variable v -> f v
    value -> do
      v <- grin . Unit $ value
      bind v f

grin :: Expression -> Grin Value
grin = singleton . Typed

class Pattern a where
  getPattern :: GrinState (a, [Variable])
  toValue :: a -> Grin Value

instance Pattern Value where
  getPattern = do
    r <- register
    return (Variable r, [r])
  toValue = return

instance Pattern Variable where
  getPattern = do
    r <- register
    return (r, [r])
  toValue = return . Variable

instance (Pattern a, Pattern b) => Pattern (a, b) where
  getPattern = do
    (v0, rs0) <- getPattern
    (v1, rs1) <- getPattern
    return ((v0, v1), rs0 ++ rs1)
  toValue (v0, v1) = bind v0 (\tag -> do
                                 value <- toValue v1
                                 return (Node tag [value]))

instance Pattern Ignore where
  getPattern = do
    r <- register
    return (I, [r])
  toValue = error "Cannot use ignored variable toValue"

instance Pattern () where
  getPattern = do
    r <- register
    return ((), [r])
  toValue = error "Cannot use ignored variable toValue"

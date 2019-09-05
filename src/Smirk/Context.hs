module Smirk.Context where

import Control.Monad.State
import Data.Map
import GHC.TypeLits

import Smirk.Syntax

data Context = Context {
  bindings_ :: Map SomeSymbol ([Variable], Expression),
  register_ :: Integer
  }

type GrinState = State Context

defaultContext :: Context
defaultContext = Context empty 0

registers :: Integer -> GrinState [Variable]
registers n = do
  r <- gets register_
  modify (\s -> s { register_ = abs n + register_ s })
  return (Prelude.map Register [r..(r+n-1)])

register :: GrinState Variable
register = do
  ~[r] <- (registers 1)
  return r

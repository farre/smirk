{-# Language GADTs, FlexibleInstances, UndecidableInstances, OverlappingInstances, MultiParamTypeClasses, FlexibleContexts, AllowAmbiguousTypes, RankNTypes, LambdaCase, TupleSections #-}
module Smirk ( module Smirk ) where

import Control.Monad.Operational hiding (singleton)
import qualified Control.Monad.Operational as Op (singleton)
import Control.Monad.State
import Data.Map hiding (map, singleton)
import Smirk.Compiler
import Smirk.Context (defaultContext, Context(..))
import Smirk.Core
import Smirk.Language
import Smirk.Pretty (pp, render)
import Smirk.Syntax

import Debug.Trace

import GHC.TypeLits

node :: Value -> [Value] -> Grin Value
node tag values = bind tag (\t -> grin (Unit (Node t values)))

var :: Integer -> Value
var = Variable . Register

reg :: Integer -> Variable
reg = Register

ref :: Variable -> Value
ref = Variable

foo :: Value -> Grin Value
foo v = match v
  (\r -> unit (ref r))
  (\I -> unit v)

bar :: Variable -> Grin Value
bar = unit . Variable

integer :: Integral n => n -> Value
integer = Number . toInteger

scoped :: MonadState s m => m a -> m a
scoped m = do
  s <- get
  r <- m
  put s
  return r

prog x y = do
  v <- node (integer 5) ([integer 5, x, y])
  w <- fetch v 0
  n <- unit (integer 8)
  x <- store w
  u <- (match n foo bar)
  t <- match (Node (reg 4) [u, v])
    (\(a, b) -> do
        node a [a, b])
    bar
  s <- match v
    (\((), b) -> unit b)
  node v [v, w, u, x, t, s]

decl = do
  unit (var 7)
  define "foo" prog

test =
  let (e, s) = runState (expression decl) defaultContext
  in do
    putStrLn "Bindings:"
    pp (bindings_ s)
    putStrLn "Main:"
    pp e

test2 = pp $ evalState (expression (foo (integer 4))) defaultContext

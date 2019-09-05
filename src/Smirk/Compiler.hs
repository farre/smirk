{-# Language LambdaCase #-}
{-# Language TupleSections #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language UndecidableInstances #-}
{-# Language OverlappingInstances #-}
module Smirk.Compiler where

import Control.Monad.Operational hiding (singleton)
import Control.Monad.State
import Data.Map hiding (singleton)

import GHC.TypeLits

import Smirk.Context
import Smirk.Core
import Smirk.Syntax

type Parameters = [Variable] -> [Variable]

class Definition f x where
  definition :: SomeSymbol -> Parameters -> f -> x

instance (Pattern a, Definition b (Grin ())) =>
  Definition (a -> b) (Grin ()) where
  definition name acc f = do
    (p, vs) <- lift getPattern
    definition name (acc . (vs ++)) (f p)

instance Definition (Grin Value) (Grin ()) where
  definition name acc x = do
    lift (expression x) >>= return . fmap (acc [],) >>=
      \case
        Nothing -> return ()
        Just bound -> modify (\s -> s { bindings_ = insert name bound (bindings_ s) } )

expression :: Grin a -> GrinState (Maybe Expression)
expression = eval <=< viewT
  where
    eval :: GrinView a -> GrinState (Maybe Expression)
    eval (Return _) = return Nothing
    eval ((Typed i) :>>= k) = do
      (p, vs) <- getPattern
      k' <- expression (k p)
      return $ maybe (Just i) (Just . (Bind i vs)) k'

alternative :: Pattern a => (a -> Grin Value) -> Grin (Maybe Binding)
alternative fn = do
  (p, vs) <- lift getPattern
  alt <- lift $ expression (fn p)
  return (alt >>= return . Binding . (vs,))

cases :: Value -> Grin [Binding] -> Grin Value
cases value alts = bind value (\v -> alts >>= (grin . (Case v)))

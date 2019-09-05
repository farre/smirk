{-# Language GADTs, DataKinds, KindSignatures #-}
module Smirk.Syntax where

import GHC.TypeLits

data Literal (a :: Nat) = Literal Value

data Value where
  Node :: Variable -> [Value] -> Value
  LiteralNode :: KnownNat n => Literal n -> [Value] -> Value
  Number :: Integer -> Value
  Variable :: Variable -> Value

data Variable where
   Register :: Integer -> Variable

type Offset = Integer

data Expression where
  Unit :: Value -> Expression
  Store :: Value -> Expression
  Fetch :: Variable -> Offset -> Expression
  Update :: Variable -> Value -> Expression
  Bind :: Expression -> [Variable] -> Expression -> Expression
  Case :: Variable -> [Binding] -> Expression

newtype Binding = Binding ([Variable], Expression)

newtype Declaration = Declaration ([Variable], Expression)

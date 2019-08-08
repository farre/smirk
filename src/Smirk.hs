{-# Language GADTs #-}
module Smirk ( compile ) where

import Control.Monad.Operational
import Control.Monad.State

newtype Store = Store ()

data Value where
  Node :: [Value] -> Value
  Number :: Integer -> Value

data Variable where
  Variable :: Integer -> Variable

data Instruction where
  Unit :: Value -> Instruction
  Bind :: Instruction -> Instruction -> Instruction
  Lambda :: [Variable] -> Instruction -> Instruction

data Typed a where
  Typed :: IsValue a => Instruction -> Typed a

type Grin = ProgramT Typed (State Store)

class IsValue a where
  toValue :: a -> Value
  toPattern :: Value -> a

compile :: IsValue a => Grin a -> State Store Instruction
compile = eval <=< viewT
  where
    eval (Return x) = return (Unit (toValue x))
    eval (i :>>= k) = do
      k' <- compile (k undefined)
      return (Bind (erase i) (lambda i k'))
    erase (Typed i) = i
    lambda i k = Lambda [] k

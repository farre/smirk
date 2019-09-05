{-# Language FlexibleInstances #-}
module Smirk.Pretty (pp, render) where

import Prelude hiding ((<>))

import Data.Map (Map, foldrWithKey)
import Text.PrettyPrint hiding (render)

import GHC.TypeLits

import Smirk.Syntax

pp :: Pretty a => a -> IO ()
pp = putStrLn . render

render :: Pretty a => a -> String
render = renderStyle style . pretty

class Pretty a where
  pretty :: a -> Doc

prettyList :: Pretty p => [p] -> Doc
prettyList = hsep . ((return $!) . pretty =<<)

instance Pretty SomeSymbol where
  pretty (SomeSymbol p) = text (symbolVal p)

instance (Pretty k, Pretty v, Pretty e) => Pretty (Map k ([v], e)) where
  pretty m = foldrWithKey (\k (vs, e) d -> hang (pretty k <+> prettyList vs <+> char '=') 4 (pretty e) $+$ d) empty m

instance Pretty Integer where
  pretty = integer

instance Pretty Variable where
  pretty (Register r) = char 'r' <> integer r

instance KnownNat n => Pretty (Literal n) where
  pretty lit = text "tag" <> (pretty (natVal lit))

instance Pretty Value where
  pretty (Node tag values) = parens $ pretty tag <+> prettyList values
  pretty (LiteralNode tag values) = parens $ pretty tag <+> prettyList values
  pretty (Number n) = integer n
  pretty (Variable v) = pretty v

group :: Pretty p => [p] -> Doc
group [] = empty
group [x] = pretty x
group xs = parens . prettyList $ xs

instance Pretty Binding where
  pretty (Binding (vs, i)) = group vs <+> text "->" <+> pretty i

instance Pretty Expression where
  pretty (Unit v) = text "unit" <+> pretty v
  pretty (Store v) = text "store" <+> pretty v
  pretty (Fetch v o) = text "fetch" <+> pretty v <+> (integer o)
  pretty (Update r v) = text "update" <+> (pretty r) <+> pretty v
  pretty (Bind f vs g) = (pretty f <+> char ';' <+> char '\\' <> group vs <+> text "->") $+$ pretty g
  pretty (Case var alts) =
    parens $ hang (text "case" <+> pretty var <+> text "of") 4 (vcat [ pretty alt | alt <- alts ])

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = text ""
  pretty (Just v) = pretty v

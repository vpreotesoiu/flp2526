module Equivalence where

import Syntax ( Expr(..), Variable )
import Substitution ( substitute, freshVar, freeVars )
import Parser (parseFirst, pexpr)

{-| Tests whether two expressions are alpha-equivalent

Examples:

>>> testAlphaEquiv "x" "x"
Right True

>>> testAlphaEquiv "x" "y"
Right False

>>> testAlphaEquiv "\\x -> x" "\\y -> y"
Right True

>>> testAlphaEquiv "\\x -> y" "\\y -> x"
Right False

>>> testAlphaEquiv "\\x -> y" "\\z -> y"
Right True

>>> testAlphaEquiv "\\x -> y" "\\y -> y"
Right False

>>> testAlphaEquiv "\\x x x x x -> x" "\\x y z t u -> u"
Right True

>>> testAlphaEquiv "\\x x x x x -> x" "\\x y z t u -> x"
Right False
-}
alphaEquiv :: Expr -> Expr -> Bool
alphaEquiv (Lambda x e) (Lambda y e')
  | x == y = e `alphaEquiv` e'
  | otherwise = substitute x (Var z) e `alphaEquiv` substitute y (Var z) e' where
      z = freshVar x (x : y : (freeVars e ++ freeVars e'))

alphaEquiv (App e1 e2) (App e1' e2') = (e1 == e1') && (e2 == e2')
alphaEquiv e1 e2 = e1 == e2

testAlphaEquiv :: String -> String -> Either String Bool
testAlphaEquiv s1 s2 = alphaEquiv <$> parse s1 <*> parse s2
  where
    parse = parseFirst pexpr

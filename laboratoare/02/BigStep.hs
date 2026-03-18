module BigStep ( bsStmt ) where

import Syntax ( AExpr(..), BExpr(..), Stmt(..) )
import State ( get, set, State )
import Configurations ( Conf(..), Conf1(..) )
import Parser ( aconf, bconf, parseFirst, sconf, Parser ) -- for testing purposes

{- big-step semantics for arithmetic expressions

Examples:

>>> testExpr "< 5 , >"
< 5 >

>>> testExpr "< x , a |-> 3, x |-> 4 >"
< 4 >

>>> testExpr "< x + a, a |-> 3, x |-> 4 >"
< 7 >

>>> testExpr "< x - a, a |-> 3, x |-> 4 >"
< 7 >

>>> testExpr "< x * a, a |-> 3, x |-> 4 >"
< 12 >
-}
bsExpr :: Conf AExpr -> Conf1 Integer
bsExpr (Conf (ENum i) sigma) = Conf1 i
bsExpr (Conf (EId x) sigma) = Conf1 (get sigma x)
bsExpr (Conf (EPlu a1 a2) sigma) = let
                                      Conf1 i1 = bsExpr (Conf a1 sigma)
                                      Conf1 i2 = bsExpr (Conf a2 sigma)
                                   in
                                      Conf1 (i1 + i2)
bsExpr (Conf (EMinu a1 a2) sigma) = let
                                      Conf1 i1 = bsExpr (Conf a1 sigma)
                                      Conf1 i2 = bsExpr (Conf a2 sigma)
                                    in
                                      Conf1 (i1 - i2)
bsExpr (Conf (EMul a1 a2) sigma) = let
                                      Conf1 i1 = bsExpr (Conf a1 sigma)
                                      Conf1 i2 = bsExpr (Conf a2 sigma)
                                    in
                                      Conf1 (i1 * i2)

{- big-step semantics for boolean expressions

Examples:
>>> testBExpr "< true, >"
< True >

>>> testBExpr "< false, >"
< False >

>>> testBExpr "< x == a, a |-> 3, x |-> 4 >"
< False >

>>> testBExpr "< a <= x, a |-> 3, x |-> 4 >"
< True >

>>> testBExpr "< !(a <= x), a |-> 3, x |-> 4 >"
< False >

>>> testBExpr "< true && false, >"
< False >

>>> testBExpr "< true || false, >"
< True >
-}
bsBExpr :: Conf BExpr -> Conf1 Bool
bsBExpr (Conf BTrue sigma) = Conf1 True
bsBExpr (Conf BFalse sigma) = Conf1 False
bsBExpr (Conf (BEq a1 a2) sigma) = Conf1 (i1 == i2)
  where
    Conf1 i1 = bsExpr (Conf a1 sigma)
    Conf1 i2 = bsExpr (Conf a2 sigma)
bsBExpr (Conf (BLe a1 a2) sigma) = Conf1 (i1 <= i2)
  where
    Conf1 i1 = bsExpr (Conf a1 sigma)
    Conf1 i2 = bsExpr (Conf a2 sigma)
bsBExpr (Conf (BNot b) sigma) = Conf1 (not t)
  where
    Conf1 t = bsBExpr (Conf b sigma)
bsBExpr (Conf (BAnd b1 b2) sigma) = Conf1 (t1 && t2)
  where
    Conf1 t1 = bsBExpr (Conf b1 sigma)
    Conf1 t2 = bsBExpr (Conf b2 sigma)
bsBExpr (Conf (BOr b1 b2) sigma) = Conf1 (t1 || t2)
  where
    Conf1 t1 = bsBExpr (Conf b1 sigma)
    Conf1 t2 = bsBExpr (Conf b2 sigma)


{- big-step semantics for statements

Examples:

>>> testStmt "< skip, >"
<  >

>>> testStmt "< x := x + 1, a |-> 3, x |-> 4 >"
< x |-> 5, a |-> 3 >

>>> testStmt "< x := x + 1; a := x + a, a |-> 3, x |-> 4 >"
< a |-> 8, x |-> 5 >

>>> testStmt "< if a <= x then max := x else max := a, a |-> 3, x |-> 4 >"
< max |-> 4, a |-> 3, x |-> 4 >

>>> testStmt "< while a <= x do x := x - a, a |-> 7, x |-> 33 >"
< x |-> 5, a |-> 7 >
-}
bsStmt :: Conf Stmt -> Conf1 State
bsStmt (Conf SSkip sigma) = Conf1 sigma
bsStmt (Conf (SAss s a) sigma) = Conf1 (set sigma s a')
  where
    Conf1 a' = bsExpr (Conf a sigma)
bsStmt (Conf (SSeq s s') sigma) = Conf1 t'
  where
    Conf1 t = bsStmt (Conf s sigma)
    Conf1 t' = bsStmt (Conf s' t)
bsStmt (Conf (SIf b s s') sigma) =
  case bsBExpr (Conf b sigma) of
    Conf1 False -> bsStmt (Conf s' sigma)
    Conf1 True -> bsStmt (Conf s sigma)
bsStmt (Conf (SWhile cnd bl) sigma) =
  case bsBExpr (Conf cnd sigma) of
    Conf1 False -> Conf1 sigma
    Conf1 True  -> case bsStmt (Conf bl sigma) of
                      Conf1 sigma' -> bsStmt (Conf (SWhile cnd bl) sigma')



-- Below are the functions used for a nice testing experience
-- they combine running the actual function being tested with parsing
-- to allow specifying the input configuration as a string
--
-- These, together with the Show instances in the Configurations, State, and Syntax modules
-- make the input and output look closer to how it would look on paper.

test :: Show c => (c -> c') -> Parser c -> String -> c'
test f p s = f c
  where
    c = case parseFirst p s of
      Right c -> c
      Left err -> error ("parse error: " ++ err)

testExpr :: String -> Conf1 Integer
testExpr = test bsExpr aconf

testBExpr :: String -> Conf1 Bool
testBExpr = test bsBExpr bconf

testStmt :: String -> Conf1 State
testStmt = test bsStmt sconf

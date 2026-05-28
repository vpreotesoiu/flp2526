module TypeCheck where
import Syntax (Variable, Type(..), Expr(..))
import Parser (Parser, commaSep0, pvar, symbol, ptype, parseFirst, pexpr)

type TypeAssignment = [(Variable, Type)]

vlookup :: TypeAssignment -> Variable -> Either String Type
vlookup assgn x = maybe (Left "variable not found") Right (lookup x assgn)

{-| Type checker

    Given a type assignment (which gives the unique type for each variable) and
    an expression `e` verifies whether there exist a type `t` such that
    the expresison `e:t` is well-formed according to the Church typing rules.

    If so, it outputs t. If not it outputs an error.

Examples:

>>> testTypeCheck "x :: (a -> b) -> c, y :: a -> b" "x y"
Right c

>>> testTypeCheck "x :: a -> a, y :: (a -> a) -> b, z :: b, u :: c" "(\\ z u -> z) (y x)"
Right c -> b

>>> testTypeCheck "x :: a -> a" "x x"
WAS WAS WAS WAS Left "x :: a -> a cannot be applied to x :: a -> a"
WAS WAS WAS NOW Left "i don't know 1"
WAS WAS NOW Left "a -> a ... a -> a"
WAS NOW Left "x :: a -> a cannot be applied to x :: a -> a"
NOW Left "x :: a -> a cannot be applied to x :: a -> a"
-}
typeCheck :: TypeAssignment -> Expr -> Either String Type
typeCheck ta (Var x) =
  do
    t <- vlookup ta x
    Right t
typeCheck ta (App e1 e2) =
  do
    tf <- typeCheck ta e1
    tx <- typeCheck ta e2
    case tf of
      TArr dom codom
        | tx == dom -> Right codom
      _ ->
        Left (show e1 ++ " :: " ++ show tf ++ " cannot be applied to " ++ show e2 ++ " :: " ++ show tx)

typeCheck ta (Lambda x e) =
  do
    te <- typeCheck ta e
    tx <- vlookup ta x
    Right (TArr tx te)


{-| parser for  a singleton variable - type pair

Examples:

>>> parseFirst psingleTypeAssgn "x :: (a -> v) -> c"
Right (x,(a -> v) -> c)
-}
psingleTypeAssgn :: Parser (Variable, Type)
psingleTypeAssgn = (,) <$> pvar <*> (symbol "::" *> ptype)

{-| parser for a TypeAssignment

Examples:
>>> parseFirst pTypeAssgn "x :: (a -> b) -> c, y :: a -> b -> c"
Right [(x,(a -> b) -> c),(y,a -> b -> c)]
-}
pTypeAssgn :: Parser TypeAssignment
pTypeAssgn = commaSep0 psingleTypeAssgn

testTypeCheck :: String -> String -> Either String Type
testTypeCheck sAssgn sExpr
  = do
    assgn <- parseFirst pTypeAssgn sAssgn
    expr <- parseFirst pexpr sExpr
    typeCheck assgn expr

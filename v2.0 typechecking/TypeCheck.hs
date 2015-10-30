module TypeCheck where

import Declare
import Interp
import Parser
import Prelude hiding (LT, GT, EQ)

data Type
  = TInt
  | TBool
  deriving (Eq,Show)

type TEnv = [(String,Type)]


-- | Question 4
--
-- >>> tunary Neg TInt
-- Just TInt
--
-- >>> tbinary Add TInt TBool
-- Nothing
tunary :: UnaryOp -> Type -> Maybe Type
--tunary = error "TODO: Question 4"
tunary Not TBool = Just TBool
tunary Not TInt = Nothing
tunary Neg TInt = Just TInt
tunary Neg TBool = Nothing


tbinary :: BinaryOp -> Type -> Type -> Maybe Type
--tbinary = error "TODO: Question 4"
tbinary Add TInt TInt = Just TInt
tbinary Add _ _ = Nothing
tbinary Sub TInt TInt = Just TInt
tbinary Sub _ _ = Nothing
tbinary Mult TInt TInt = Just TInt
tbinary Mult _ _ = Nothing
tbinary Div TInt TInt = Just TInt
tbinary Div _ _ = Nothing
tbinary Power TInt TInt = Just TInt 
tbinary Power _ _ = Nothing
tbinary And TBool TBool = Just TBool
tbinary And _ _ = Nothing
tbinary Or TBool TBool = Just TBool
tbinary Or _ _ = Nothing
tbinary GT TInt TInt = Just TBool
tbinary GT _ _ =Nothing 
tbinary LT TInt TInt = Just TBool
tbinary LT _ _ =Nothing 
tbinary GE TInt TInt = Just TBool
tbinary GE _ _ =Nothing 
tbinary LE TInt TInt = Just TBool
tbinary LE _ _ =Nothing 
tbinary EQ TInt TInt = Just TBool
tbinary EQ TBool TBool = Just TBool
tbinary EQ _ _ = Nothing




-- | Question 5
--
-- >>> testq4 "1"
-- Just TInt
--
-- >>> testq4 "false"
-- Just TBool
--
-- >>> testq4 "1*false"
-- Nothing
--
-- >>> testq4 "var x = 5; if (x > 0) x; else x * x"
-- Just TInt
--
-- >>> testq4 "var x = y; var y = 3; x + y"
-- Nothing
tcheck :: Exp -> TEnv -> Maybe Type
--tcheck = error "TODO: Question 4"
tcheck (Literal (IntV x)) tenv= Just TInt
tcheck (Literal (BoolV bool)) tenv= Just TBool
tcheck (Unary op exp) tenv= case (tcheck exp tenv) of
  Just t -> tunary op t
  Nothing ->Nothing

tcheck (Binary op exp1 exp2) tenv= 
        case (tcheck exp1 tenv) of
          Nothing ->Nothing
          Just t1 -> case (tcheck exp2 tenv) of
                      Nothing ->Nothing
                      Just t2 -> tbinary op t1 t2


tcheck (If exp1 exp2 exp3) tenv =
  if((tcheck exp1 tenv) /= (Just TBool)) then Nothing
  else 
    if ((tcheck exp2 tenv)==(tcheck exp3 tenv)) then (tcheck exp2 tenv)
      else Nothing

tcheck (Var v) tenv = case lookup v tenv of
  Nothing -> Nothing
  Just t -> Just t

tcheck (Decl var exp1 exp2) tenv = case (tcheck exp1 tenv) of
  Nothing -> Nothing
  Just t -> if ((fv (Decl var exp1 exp2)) /= []) then Nothing
            else  let tenv' = (var, t):tenv in
            tcheck exp2 tenv'


        
-- | Question 6
--
-- >>> tcalc "3 == 3"
-- true
--
-- >>> tcalc "if (3 == 4) true; else false"
-- false
--
-- >>> tcalc "var x = 3; x + true"
-- *** Exception: You have a type-error in your program!
tcalc :: String -> Value
--tcalc = error "TODO: Question 5"
tcalc str = case (tcheck (parseExpr str) []) of
            Nothing -> error "You have a type-error in your program!"
            Just t -> evaluate (parseExpr str)



testq4 :: String -> Maybe Type
testq4 e = tcheck (parseExpr e) []

module Interp where

import Parser
import Declare
import Data.List (delete, union, sort)
import Prelude hiding (LT, GT, EQ)


--data Exp = Literal Value
  --       | Unary UnaryOp Exp
    --     | Binary BinaryOp Exp Exp
      --   | If Exp Exp Exp
        -- | Var String
        -- | Decl String Exp Exp




-- | Question 1
--
-- >>> testq1 "1 + 2"
-- []
--
-- >>> testq1 "x * x"
-- ["x"]
--
-- >>> testq1 "var x = 3; x * y * z"
-- ["y","z"]
--
-- >>> testq1 "var x = y; var y = 3; x + y"
-- ["y"]
fv :: Exp -> [String]
--fv = error "TODO: Question 1"
fv (Literal x) = []
fv (Unary op exp) = fv exp
fv (Binary op exp1 exp2) = union (fv exp1) (fv exp2)
fv (If exp1 exp2 exp3) = union (union (fv exp1) (fv exp2)) (fv exp3)
fv (Var x) =  [x]
fv (Decl var exp1 exp2) = union (fv exp1) (delete var (fv exp2)) 

-- | Question 2
--
-- >>> unary Not (BoolV True)
-- false
--
-- >>> unary Neg (IntV 3)
-- -3
--
-- >>> binary Add (IntV 2) (IntV 3)
-- 5
unary :: UnaryOp -> Value -> Value
--unary = error "TODO: Question 2"
unary Not (BoolV bool) = BoolV (not bool) 
unary Not (IntV x) = error "Type error for Not operator"
unary Neg (IntV x) = IntV (-x)
unary Neg (BoolV bool) = error "Type error for Neg operator"

binary :: BinaryOp -> Value -> Value -> Value
--binary = error "TODO: Question 2"

binary Sub (IntV a) (IntV b) = IntV (a-b)
binary Sub _ _ = error "Type error for Sub"


binary Mult (IntV a) (IntV b) = IntV (a*b)
binary Mult _ _ = error "Type error for Mult"


binary Add (IntV a) (IntV b) =IntV (a+b)
binary Add _ _ = error "Type error for Add"


binary Div (IntV a) (IntV b) = 
      if(b==0) then error "Divide by 0"
      else IntV (a `div` b)
binary Div _ _ = error "Type error for Div"

binary Power (IntV a) (IntV b) = 
      if(b<0) then error "Raised to negarive number"
      else IntV (a^b)
binary Power _ _ = error "Type error for Power"

binary And (BoolV x) (BoolV y) = BoolV (x&&y)
binary And _ _ = error "Type error for And"

binary Or (BoolV x) (BoolV y) = BoolV (x||y)
binary Or _ _ = error "Type error for Or"

binary GT (IntV a) (IntV b) = BoolV (a>b)
binary GT _ _ = error "Type error for GT"

binary LT (IntV a) (IntV b) = BoolV (a<b)
binary LT _ _ = error "Type error for LT"

binary GE (IntV a) (IntV b) = BoolV(a>=b)
binary GE _ _ = error "Type error for GE"

binary LE (IntV a) (IntV b) = BoolV (a<=b)
binary LE _ _ = error "Type error for LE"

binary EQ (IntV a) (IntV b) = BoolV (a==b)
binary EQ (BoolV x) (BoolV y) = BoolV (x==y)
binary EQ _ _ = error "Type error for EQ"


type Binding = (String, Value)
type Env = [Binding]


-- | Question 3
--
-- >>> calc "1 + 2"
-- 3
--
-- >>> calc "if (true) 1; else 3"
-- 1
--
-- >>> calc "var x = 5; if (x > 0) x; else x * x"
-- 5
evaluate :: Exp -> Value
evaluate e = eval e [] -- starts with an empty environment
  where eval :: Exp -> Env -> Value
        --eval = error "TODO: Question 3"
        eval (Literal x) env= x
        eval (Unary op exp) env = unary op (eval exp env)
        eval (Binary op exp1 exp2) env = binary op (eval exp1 env) (eval exp2 env)
        eval (If exp1 exp2 exp3) env = 
          let test = eval exp1 env in
            if((show test == "true")) then eval exp2 env
                           else eval exp3 env
        
        eval (Var x) env = case lookup x env of
          Just a -> a
          Nothing -> error "Not Found variable" 
        
        eval (Decl var exp1 exp2) env= 
          case lookup var env of
            Just a -> let env' = (var,(eval exp1 env)) : (delete (var,a) env) in eval exp2 env'
            Nothing -> let env' = (var,(eval exp1 env)):env in eval exp2 env'

calc :: String -> Value
calc = evaluate . parseExpr




testq1 :: String -> [String]
testq1 = sort . fv . parseExpr

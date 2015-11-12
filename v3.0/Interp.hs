module Interp where

import Declare
import Prelude hiding (LT, GT, EQ)

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i)  = IntV (-i)

binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV a) (IntV b) = IntV (a + b)
binary Sub (IntV a) (IntV b) = IntV (a - b)
binary Mult (IntV a) (IntV b) = IntV (a * b)
binary Div (IntV a) (IntV b) = IntV (a `div` b)
binary Power (IntV a) (IntV b) = IntV (a ^ b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or (BoolV a) (BoolV b) = BoolV (a || b)
binary LT (IntV a) (IntV b) = BoolV (a < b)
binary LE (IntV a) (IntV b) = BoolV (a <= b)
binary GE (IntV a) (IntV b) = BoolV (a >= b)
binary GT (IntV a) (IntV b) = BoolV (a > b)
binary EQ a b = BoolV (a == b)

type Binding = (String, Value)
type Env = [Binding]

execute :: Program -> Value
execute (Program funEnv main) = evaluate main [] funEnv

evaluate :: Exp -> Env -> FunEnv -> Value
evaluate e en fenv = eval e en
  where
    eval :: Exp -> Env -> Value
--    eval (Call fun args) env = error "TODO: Question 5"
    eval (Call fun args) env =
     let (Just (Function xs body)) = lookup fun fenv
         values = map (\e->eval e env) args
         newEnv = zip (map (\(arg,t)->arg) xs) values
     in eval body newEnv
    eval (Lit n) _ = n
    eval (Unary op ex) env = unary op (eval ex env)
    eval (Bin op e1 e2) env = binary op (eval e1 env) (eval e2 env)
    eval (If e1 e2 e3) env =
      let BoolV test = eval e1 env
      in if test
           then eval e2 env
           else eval e3 env
    eval (Var v) env =
      case lookup v env of
        Just x  -> x
        Nothing -> error "Not found"
    eval (Decl v a b) env =
      let a' = eval a env
          env' = (v, a') : env
      in eval b env'

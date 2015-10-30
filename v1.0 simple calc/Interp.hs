module Interp where

import Parser
import Declare
import Prelude hiding (Either(..))

--Error Handlingd
data Either a b = Left a | Right b deriving Show
--use as
safeHead :: [a]->Either String a
safeHead [] = Left "Can't access the head of an empty list"
safeHead (x:_) = Right x
--



bindE :: Either a b ->(b->Either a b)->Either a b
bindE (Left msg) f = Left msg
bindE (Right a') f = f a'

evaluate :: Exp ->Either String Int

evaluate (Num n) = Right n
evaluate (Add a b) = 
  bindE (evaluate a) (\a'->
    bindE (evaluate b) (\b'->Right(a'+b')))

evaluate (Sub a b) = 
  bindE (evaluate a) (\a' ->
    bindE (evaluate b) (\b' -> Right (a'-b')))
evaluate (Mult a b) = 
  bindE (evaluate a) (\a'->
    bindE (evaluate b) (\b'-> Right (a'*b')))

evaluate (Div a b) = 
  bindE (evaluate a) (\a'->
    bindE (evaluate b) (\b'-> (
     if b'== 0 then  Left ("Divide by zero: "++ showExpr b)
     else Right (div a' b'))))

evaluate (Neg a) = 
  bindE (evaluate a) (\a'-> Right (negate a'))

evaluate (Power a b) = 
  bindE (evaluate a) (\a'->
    bindE (evaluate b) (\b'-> (
      if b'<0 then  Left ("Rasied to negative number: "++(showExpr b))
      else  Right (a' ^ b'))))


calc :: String ->Either String Int
calc = evaluate .parseExpr

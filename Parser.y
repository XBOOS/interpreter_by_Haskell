{
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (stripPrefix)
import Declare (Exp(..))
}


%name parser
%tokentype { Token }
%error { parseError }

%token
    int     { TokenInt $$ }
    '+'     { TokenSymbol "+" }
    '-'     { TokenSymbol "-" }
    '*'     { TokenSymbol "*" }
    '/'     { TokenSymbol "/" }
    '^'     { TokenSymbol "^" }
    '('     { TokenSymbol "(" }
    ')'     { TokenSymbol ")" }

%%

AExpr : AExpr '+' Term        { Add $1 $3 }
      | AExpr '-' Term        { Sub $1 $3 }
      | Term                  { $1 }

Term : Term '*' Factor        { Mult $1 $3 }
     | Term '/' Factor        { Div $1 $3 }
     | Factor                 { $1 }

Factor : Expon '^' Factor     { Power $1 $3}
       | Expon                { $1 }

Expon : '-' Expon             { Neg $2 }
      | Primary               { $1 }

Primary : int                 { Num $1 }
        | '(' AExpr ')'       { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenInt Int
      | TokenKeyword String
      | TokenSymbol String
      | TokenIdent String

lexer :: [String] -> [String] -> String -> [Token]
lexer symbols keywords = lexer'
  where lexer' [] = []
        lexer' s@(c:cs)
          | isSpace c = lexer' cs
          | isDigit c = lexNum s
          | isAlpha c = lexVar s
          | otherwise = lexSym s symbols

        lexNum cs = TokenInt (read num) : lexer' rest
          where (num, rest) = span isDigit cs

        lexVar cs = token : lexer' rest
          where (var, rest) = span isAlpha cs
                token = if var `elem` keywords
                        then TokenKeyword var
                        else TokenIdent var

        lexSym cs (s:ss) = case stripPrefix s cs of
                            Just rest -> TokenSymbol s : lexer' rest
                            Nothing -> lexSym cs ss
        lexSym cs [] = error $ "Cannot tokenize " ++ cs

symbols = ["+", "-", "*", "/", "(", ")", "^"]
keywords = []

parseExpr = parser . lexer symbols keywords

}

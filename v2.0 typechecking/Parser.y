{
module Parser (parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Data.List (stripPrefix)
import Declare
}


%name parser
%tokentype { Token }
%error { parseError }

%token
    var     { TokenKeyword "var" }
    id      { TokenIdent $$ }
    int     { TokenInt $$ }
    if      { TokenKeyword "if"}
    else    { TokenKeyword "else"}
    true    { TokenKeyword "true"}
    false   { TokenKeyword "false"}
    '+'     { TokenSymbol "+" }
    '-'     { TokenSymbol "-" }
    '*'     { TokenSymbol "*" }
    '/'     { TokenSymbol "/" }
    '^'     { TokenSymbol "^" }
    '('     { TokenSymbol "(" }
    ')'     { TokenSymbol ")" }
    ';'     { TokenSymbol ";" }
    '='     { TokenSymbol "=" }
    '<'     { TokenSymbol "<" }
    '<='    { TokenSymbol "<=" }
    '>'     { TokenSymbol ">" }
    '>='    { TokenSymbol ">=" }
    '=='    { TokenSymbol "==" }
    '&&'    { TokenSymbol "&&" }
    '!'     { TokenSymbol "!" }
    '||'    { TokenSymbol "||" }

%%

Exp : var id '=' Exp ';' Exp           { Decl $2 $4 $6 }
    | if '(' Exp ')' Exp ';' else Exp  { If $3 $5 $8 }
    | Or                               { $1 }

Or   : Or '||' And        { Binary Or $1 $3 }
     | And                { $1 }

And  : And '&&' Comp      { Binary And $1 $3 }
     | Comp                { $1 }

Comp : AExpr '==' AExpr     { Binary EQ $1 $3 }
     | AExpr '<' AExpr      { Binary LT $1 $3 }
     | AExpr '>' AExpr      { Binary GT $1 $3 }
     | AExpr '<=' AExpr     { Binary LE $1 $3 }
     | AExpr '>=' AExpr     { Binary GE $1 $3 }
     | AExpr              { $1 }

AExpr : AExpr '+' Term        { Binary Add $1 $3 }
      | AExpr '-' Term        { Binary Sub $1 $3 }
      | Term                  { $1 }

Term : Term '*' Factor        { Binary Mult $1 $3 }
     | Term '/' Factor        { Binary Div $1 $3 }
     | Factor                 { $1 }

Factor : Expon '^' Factor     { Binary Power $1 $3}
       | Expon                { $1 }

Expon : '-' Expon             { Unary Neg $2 }
      | '!' Expon             { Unary Not $2 }
      | Primary               { $1 }

Primary : int                 { Literal (IntV $1) }
        | true                { Literal (BoolV True) }
        | false               { Literal (BoolV False) }
        | id                  { Var $1 }
        | '(' Exp ')'         { $2 }

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

symbols = ["{", "}", ",", "+", "-", "*", "/", "(", ")", ";", "==", "=", "<=", ">=", "<", ">", "||", "&&", "!", "^"]
keywords = ["function", "var", "if", "else", "true", "false"]

parseExpr = parser . lexer symbols keywords

}

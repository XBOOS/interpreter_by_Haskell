# Interpreter\_by\_Haskell
A simple interpreter for  self-defined programming language implemented by Haskell.  
It will be implemented in a progressive way with different features and datatypes added. 

The rough idea is that concrete syntax in a specific environment will be parsed into abstract syntax(such as an abstract syntax tree),and then be evaluated to the final value. 

Like in almost all programming languages, the object language will have data of various types and functions to manipulate the data.Mechanisms like exception handlilike exception handling.  

# Parser
Wring a parser is tedious but lots of parser generators come onto stage to help automate the work.Here I will use Happy.
The grammar to feed the parser ,which is a set of rules that will spefify valid combinations of tokens to form expressions of a language,will be in a .y file.

Happy will take this grammar file and generate a Haskell module that provides a parser called parseExpr.

```bash
happy Parser.y #to generate the parser Parser.hs
```

# Process

+ version1.0  -  a simple calculator.  
	Input: calc command+ String of arithmetic expressions.
	Output: the Either value with Left String|Right Int
	
	```bash
	calc "3+4"
	Right 7
	
	calc "-8/0"
	Left "Divide by zero:0"
	
	calc "(-3)^(-3)"
	Left "Raised to negative number: (-3)"
	
	calc "4*8-6/2"
	Right 29
	```

+ version2.0 - adding local variables
	The Value are just 



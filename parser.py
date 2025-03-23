from typing import List
from enum import Enum
import re, sys

class AtomType(Enum):
    NUMBER = "Number"
    STRING = "String"
    BOOLEAN = "Boolean"
    CHARACTER = "Character"
    SYMBOL = "Symbol"
    LIST = "List"
    
class BuiltIn(Enum):
    QUOTE = "Comment"
    IF = "If statement"
    COND = "Conditional statement"
    PROGN = "Sequential execution"
    LET = "Local assignment"
    LETSEQ = "Sequential local Assignment"
    SETQ = "Global assignment"
    LAMBDA = "Anonymous function"
    DEFUN = "Define function"
    DEFMACRO = "Define MACRO"
    
    ADD = "+"
    SUB = "-"
    MULT = "*"
    DIV = "/"
    LEQ = "<="

# Used to match symbols to their AtomType/SpecialForm
reservedSymbols = {
    # Atoms
    "T" : AtomType.BOOLEAN,
    "NIL" : AtomType.BOOLEAN,
    
    # Built-In Functions
    "+" : BuiltIn.ADD,
    "-" : BuiltIn.SUB,
    "*" : BuiltIn.MULT,
    "<=" : BuiltIn.LEQ,
    
    # Special Forms
    "quote" : BuiltIn.QUOTE,
    "if" : BuiltIn.IF,
    "cond" : BuiltIn.COND,
    "progn" : BuiltIn.PROGN,

    "let" : BuiltIn.LET,
    "let*" : BuiltIn.LETSEQ,
    "setq" : BuiltIn.SETQ,    
    
    # Define functions/cacros
    "lambda" : BuiltIn.LAMBDA,
    "defun" : BuiltIn.DEFUN,
    "defmacro" : BuiltIn.DEFMACRO,
}

class Node: 
    def __init__(self, val, type, children : List = None):
        self.val = val
        self.type = type
        if not children:
            self.children = []
        else:
            self.children = children
        
    def __repr__(self):
        return self.tree_to_string()
    
    def tree_to_string(self, level=0):
        # Initialize an empty string to store the tree representation
        tree_str = '  ' * level + f'({self.type}) {self.val}\n'
        
        # Recursively add all the children to the string
        for child in self.children:
            tree_str += child.tree_to_string(level + 1)
        
        return tree_str

class Parser:
    class ParseList(list): 
        def mpop(self, numPops: int = 0):
            res = self.pop(0)
            for _ in range(1, numPops):
                self.pop(0)
            return res
    
    def __init__(self):
        self.tokenList = []
        self.parsedExpressions = []
    
    # Tokenize a string expression then convert to AST
    def parseString(self, expression: str) -> Node:
        tokenizedExpression = Parser.ParseList(self.tokenizeString(expression))
        parsedExpression = self.parseExpression(tokenizedExpression)
        self.parsedExpressions.append(expression)
        return parsedExpression

    # Tokenize a file 
    def parsefile(self, filename: str) -> List[Node]:
        tokenizedFile = Parser.ParseList(self.tokenizeFile(filename))
        
        paren = 0
        tokenizedExpressions = []
        currentExpression = Parser.ParseList([])
        # Track paren to know when a new expression begins
        for token in tokenizedFile:
            currentExpression.append(token)
            if token == "(":
                paren += 1
            elif token == ")":
                paren -= 1
                # Record new expression
                if paren == 0:
                    tokenizedExpressions.append(currentExpression)
                    currentExpression = Parser.ParseList([])
        
        # Print the tokenized expressions
        parsedExpressions = []
        for expression in tokenizedExpressions:
            parsedExpression = self.parseExpression(expression)
            print(parsedExpression)
        
        return parsedExpressions
        
    TOKEN_REGEX = re.compile(r"""
        \s+|                      # Whitespace (ignored)
        ;.*|                      # Comments (ignored)
        [-+]?\d+(\.\d*)?|         # Numbers (integers and decimals)
        [()']|                    # Parentheses and quote
        "[^"]*"|                  # Strings
        [^\s()'";]+               # Symbols
        """, re.VERBOSE)

    def tokenizeFile(self, filename):
        """Generates tokens from a Lisp source file."""
        with open(filename, "r", encoding="utf-8") as file:
            for line in file:
                for match in self.TOKEN_REGEX.finditer(line):
                    token = match.group().strip()
                    if not token or token.startswith(";"):  # Ignore whitespace and comments
                        continue
                    yield token
    
    # Turn string expression into a list of tokens
    def tokenizeString(self, expression: str) -> List[str]:
        pattern = r'\"[^\"]*\"|\(|\)|[^\s()]+'
        expression = expression.replace("'", " (quote ")
        expression = expression.replace("(", " ( ").replace(")", " ) ")
        return re.findall(pattern, expression)
    
    # Create AST given a tokenized expression
    def parseExpression(self, expression: ParseList) -> Node:
        nextToken = expression.mpop()

        # Start of new expression
        if nextToken == "(":
            if expression[0] == "defun":
                self.parseFn(expression)

            if expression[0] == "cond":
                return self.parseCond(expression)
            
            # Pop next token as operator
            nextToken = expression.mpop()
            subExpression = self.tokenToNode(nextToken)

            # Recursively process sub-expressions
            while expression[0] != ")":
                subExpression.children.append(self.parseExpression(expression))
            
            # Pop closing parentheses
            expression.mpop()
            return subExpression
        # Inside expression
        else:
            return self.tokenToNode(nextToken)
        
    # Given a token, return a node representing it
    def tokenToNode(self, nextToken) -> Node:
        numberPattern = r'^-?\d+(\.\d+)?$'
        if nextToken in reservedSymbols:
            return Node(nextToken, reservedSymbols[nextToken])
        elif nextToken[0] == "\"" and nextToken[-1] == "\"":
            return Node(nextToken, AtomType.STRING)
        elif re.fullmatch(numberPattern, nextToken):
            return Node(int(nextToken), AtomType.STRING)
        return Node(nextToken, AtomType.SYMBOL)
    
    def parseCond(self, expression: ParseList) -> Node:
        # Pop off "cond" and "(" 
        expression.mpop(2)
        
        # Recursively append condition : result pairs to children
        condNode = Node("cond", BuiltIn.COND) 
        while expression and expression[0] != "t":
            condNode.children.append(self.parseExpression(expression))
            condNode.children.append(self.parseExpression(expression))
            
            # Pop off the ')' '(' and enter next condition 
            expression.mpop(2)
        
        # Append else condition
        if expression and expression[0] == "t":
            # Pop off 't' and append else result
            expression.mpop()
            condNode.children.append(self.parseExpression(expression))

        return condNode

    def parseFn(self, expression: ParseList) -> Node:
        # Pop next three tokens : [defun, funcName, (]
        expression.mpop()
        fnName = expression.mpop()
        expression.mpop()

        # Pop arguments 
        arguments = []
        while expression[0] != ")":
            arguments.append(expression.mpop())
        expression.mpop()

        # Pop doc string
        nextToken = expression[0]
        docstring = ""
        if nextToken[0] == "\"" and nextToken[-1] == "\"":
            docstring = nextToken
            print("Docstring is:", nextToken)
            expression.mpop()
        
        # Build function node
        newFn = Node("defun", BuiltIn.DEFUN, [fnName, arguments, docstring])
        expression.mpop()
        return newFn    
    
def testParseString():
    myParser = Parser()
    ast = myParser.parseString("(defun factorial (n) \"Computes the factorial of a number recursively.\" (if (<= n 1) 1 (* n (factorial (- n 1)))))")
    ast.print_tree()

    ast = myParser.parseString("(defun fibonacci (n) \"Computes the nth Fibonacci number using recursion.\" (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))")
    ast.print_tree()
    
    # ast = myParser.runParseString("(defun filter (pred lst) \"Filters a list based on a predicate function.\" (cond ((null lst) '()) ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst)))) (t (filter pred (cdr lst)))))")
    # ast.print_tree()
    
    ast = myParser.parseString("(cond ((> x 10) \"Greater than 10\") ((< x 10) \"Less than 10\") ((= x 10) \"Equal to 10!\") (t 'other))")
    ast.print_tree()

def testParseFile(filename: str):
    myParser = Parser()
    myParser.parsefile(filename)

if __name__ == "__main__":
    if "--file" in sys.argv:
        print("Parsing file...")
        fileIndex = sys.argv.index('--file')
        if fileIndex + 1 < len(sys.argv):
            filename = sys.argv[fileIndex + 1]
            testParseFile(filename)
    else:
        print("No file, parsing default...")
        testParseString()
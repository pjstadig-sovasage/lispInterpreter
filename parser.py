from typing import List
from enum import Enum
import re

class AtomType(Enum):
    NUMBER = "Number"
    STRING = "String"
    BOOLEAN = "Boolean"
    CHARACTER = "Character"
    SYMBOL = "Symbol"
    
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
    
    def print_tree(self, level=0):
        # Print the current node with an indentation based on its level in the tree
        print('  ' * level + f'({self.type}) {self.val}')
        
        # Recursively print all the children
        for child in self.children:
            child.print_tree(level + 1)

class Parser:
    class ParseList(list): 
        def mpop(self, numPops: int):
            res = self.pop(0)
            for _ in range(1, numPops):
                self.pop(0)
            return res
    
    def __init__(self):
        self.tokenList = Parser.ParseList([])
    
    # Tokenize an expression then convert to AST
    def runParse(self, expression: str) -> Node:
        self.tokenize(expression)
        
        return self.parse()

    # Turn string expression into a list of tokens
    def tokenize(self, expression: str):
        pattern = r'\"[^\"]*\"|\(|\)|[^\s()]+'
        tokens = Parser.ParseList(re.findall(pattern, expression))
        print(tokens)
        self.tokenList = tokens

    # Create AST given a tokenized expression
    def parse(self) -> Node:
        nextToken = self.tokenList.mpop(1)

        # Start of new expression
        if nextToken == "(":
            if self.tokenList[0] == "defun":
                self.parseFn()

            if self.tokenList[0] == "cond":
                return self.parseCond()
            
            # Pop next token as operator
            nextToken = self.tokenList.pop(0)
            subExpression = self.tokenToNode(nextToken)

            # Recursively process sub-expressions
            while self.tokenList[0] != ")":
                subExpression.children.append(self.parse())
            
            # Pop closing parentheses
            self.tokenList.pop(0)
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
    
    def parseCond(self) -> Node:
        # Pop off "cond" and "(" 
        self.tokenList.pop(0)
        self.tokenList.pop(0)
        
        print(self.tokenList)
        condNode = Node("cond", BuiltIn.COND)
        
        while self.tokenList[0] != "t":
            condition = self.parse()
            result = self.parse()
            
            condNode.children.append(condition)
            condNode.children.append(result)
            self.tokenList.pop(0)
            self.tokenList.pop(0)
            print(self.tokenList)

        return condNode

    def parseFn(self) -> Node:
        # Pop next three tokens : [defun, funcName, (]
        self.tokenList.pop(0)
        fnName = self.tokenList.pop(0)
        self.tokenList.pop(0)

        # Pop arguments 
        arguments = []
        while self.tokenList[0] != ")":
            arguments.append(self.tokenList.pop(0))
        self.tokenList.pop(0)

        # Pop doc string
        nextToken = self.tokenList[0]
        docstring = ""
        if nextToken[0] == "\"" and nextToken[-1] == "\"":
            docstring = nextToken
            print("Docstring is:", nextToken)
            self.tokenList.pop(0)
        
        # Build function node
        newFn = Node("defun", BuiltIn.DEFUN, [fnName, arguments, docstring])
        self.tokenList.pop(0)
        return newFn

if __name__ == "__main__":
    myParser = Parser()
    ast = myParser.runParse("(defun factorial (n) \"Computes the factorial of a number recursively.\" (if (<= n 1) 1 (* n (factorial (- n 1)))))")
    ast.print_tree()

    ast = myParser.runParse("(defun fibonacci (n) \"Computes the nth Fibonacci number using recursion.\" (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))")
    ast.print_tree()
    
    # ast = myParser.runParse("(defun filter (pred lst) \"Filters a list based on a predicate function.\" (cond ((null lst) '()) ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst)))) (t (filter pred (cdr lst)))))")
    # ast.print_tree()
    
    ast = myParser.runParse("(cond ((> x 10) \"Greater than 10\") ((< x 10) \"Less than 10\") ((= x 10) \"Equal to 10!\") (t 'other))")
    ast.print_tree()

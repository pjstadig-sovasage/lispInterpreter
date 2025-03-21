from typing import List
from enum import Enum
import re

class AtomType(Enum):
    NUMBER = "Number"
    STRING = "String"
    BOOLEAN = "Boolean"
    SYMBOL = "Symbol"
    IF = "if"
    FUNCTION = "Function"

Keywords = {
    "T" : AtomType.BOOLEAN,
    "NIL" : AtomType.BOOLEAN,
    "if" : AtomType.IF,
    "defun" : AtomType.FUNCTION
}

class Node: 
    def __init__(self, val: any, type: AtomType):
        self.val = val
        self.type = type
        self.children = []
    
    def print_tree(self, level=0):
        # Print the current node with an indentation based on its level in the tree
        print('  ' * level + f'({self.type}) {self.val}')
        
        # Recursively print all the children
        for child in self.children:
            child.print_tree(level + 1)

class Function:
    def __init__(self, name: str, docstring: str, arguments: List[str]):
        self.name = name
        self.docstring = docstring
        self.arguments = arguments
        self.root = None
    
    def __repr__(self):
        return f"Function: {self.name} Arguments: {self.arguments} \n{self.docstring}"

class Parser:
    def __init__(self):
        self.funcTable = {}
    
    # Tokenize an expression then convert to AST
    def runParse(self, expression: str) -> Node:
        tokens = self.tokenize(expression)
        print(tokens)
        ast = self.parse(tokens)

        return ast

    # Turn string expression into a list of tokens
    def tokenize(self, expression: str) -> List[str]:
        pattern = r'\"[^\"]*\"|\(|\)|[^\s()]+'
        tokens = re.findall(pattern, expression)

        return tokens

    # Create AST given a tokenized expression
    def parse(self, tokenList: List[str]) -> Node:
        nextToken = tokenList.pop(0)

        # Start of new expression
        if nextToken == "(":
            if tokenList[0] == "defun":
                self.addFunc(tokenList)

            # Pop next token as operator
            subExpression = Node(val = tokenList.pop(0), type = "operator")

            # Recursively process sub-expressions
            while tokenList[0] != ")":
                subExpression.children.append(self.interpret(tokenList))
            
            # Pop closing parentheses
            tokenList.pop(0)
            return subExpression
        # Inside expression, so return an Atom 
        else:
            numberPattern = r'^-?\d+(\.\d+)?$'
            type = AtomType.SYMBOL
            if nextToken in Keywords:
                type = Keywords[nextToken]
            elif nextToken[0] == "\"" and nextToken[-1] == "\"":
                type = AtomType.STRING
            elif re.fullmatch(numberPattern, nextToken):
                return Node(val = nextToken, type = AtomType.NUMBER)
            return Node(nextToken, type)
    
    def addFunc(self, tokenList: List[str]) -> Function:
        # Pop next three tokens : [defun, funcName, (]
        tokenList.pop(0)
        funcName = tokenList.pop(0)
        tokenList.pop(0)

        # Pop arguments 
        arguments = []
        while tokenList[0] != ")":
            arguments.append(tokenList.pop(0))
        tokenList.pop(0)

        # Pop doc string
        nextToken = tokenList[0]
        docstring = ""
        if nextToken[0] == "\"" and nextToken[-1] == "\"":
            docstring = nextToken
            print("Docstring is:", nextToken)
            tokenList.pop(0)
        
        # Create function
        newFunc = Function(funcName, docstring, arguments)
        tokenList.pop(0)

        # Add to funcTable
        self.funcTable[funcName : newFunc]
        return newFunc

if __name__ == "__main__":
    myParser = Parser()
    ast = myParser.runParse("(if (<= n 1) 1 (* n (factorial (- n 1))))")
    ast.print_tree()

    ast = myParser.runParse("(defun fibonacci (n) \"Computes the nth Fibonacci number using recursion.\" (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))")
    ast.print_tree()
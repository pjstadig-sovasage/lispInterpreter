from typing import List
from tokens import AtomType, BuiltIn, reservedSymbols
from node import Node
import re

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

    # Parse a list of expressions
    def parseExpressionList(self, tokenizedExpressions: List[List[str]]) -> List[Node]:        
        # Print the tokenized expressions
        parsedExpressions = []
        for expression in tokenizedExpressions:
            parsedExpressions.append(self.parseExpression(expression))
        
        return parsedExpressions
    
    # Convert to parseList and pass to parse method
    def parseExpression(self, expression: List[str]) -> Node:
        return self.parse(self.ParseList(expression))
    
    # Create AST given a tokenized expression
    def parse(self, expression: ParseList) -> Node:
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
                subExpression.children.append(self.parse(expression))
            
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
    
    # Parse the "cond" special form
    def parseCond(self, expression: ParseList) -> Node:
        # Pop off "cond" and "(" 
        expression.mpop(2)
        
        # Recursively append condition : result pairs to children
        condNode = Node("cond", BuiltIn.COND) 
        while expression and expression[0] != "t":
            condNode.children.append(self.parse(expression))
            condNode.children.append(self.parse(expression))
            
            # Pop off the ')' '(' and enter next condition 
            expression.mpop(2)
        
        # Append else condition
        if expression and expression[0] == "t":
            # Pop off 't' and append else result
            expression.mpop()
            condNode.children.append(self.parse(expression))

        return condNode

    # Parse the "function" form
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
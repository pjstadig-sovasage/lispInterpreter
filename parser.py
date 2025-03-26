from typing import List, Dict, Callable
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
        
        self.specialForms: Dict[str, Callable[[Parser.ParseList], Node]]= {
            "defun": self.parseFn,
            "cond": self.parseCond,
            "quote": self.parseQuote,
            "'" : self.parseQuote
        }

    # Parse a list of expressions
    def parseExpressionList(self, tokenizedExpressions: List[List[str]]) -> List[Node]:
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
            # Pop operator and check if special
            operator = expression.mpop()
            if operator in self.specialForms:
                return self.specialForms[operator](expression)
            operator = self.tokenToNode(operator)

            # Recursively process arguments
            while expression[0] != ")":
                operator.children.append(self.parse(expression))
            
            # Pop closing parentheses
            expression.mpop()
            return operator
        # Check for special form
        elif nextToken in self.specialForms:
            return self.specialForms[nextToken](expression)
        # Inside expression
        else:
            return self.tokenToNode(nextToken)
        
    # Given a token, return a node representing it
    def tokenToNode(self, nextToken) -> Node:
        numberPattern = r'[-+]?\d+(\.\d*)?'
        if nextToken in reservedSymbols:
            return Node(nextToken, reservedSymbols[nextToken])
        elif nextToken[0] == "\"" and nextToken[-1] == "\"":
            return Node(nextToken, AtomType.STRING)
        elif re.fullmatch(numberPattern, nextToken):
            return Node(int(nextToken), AtomType.NUMBER)
        return Node(nextToken, AtomType.SYMBOL)
    
    # Parse the conditional special form
    def parseCond(self, expression: ParseList) -> Node:
        # Pop off "cond"
        expression.mpop()
        
        # Recursively append condition : result pairs to children
        condNode = Node("cond", BuiltIn.COND) 
        while expression and expression[0] != "t":
            condition = self.parse(expression)
            result = self.parse(expression)
            
            condNode.children.append(condition) 
            condNode.children.append(result) 
            
            # Pop off the ')' '(' and enter next condition 
            expression.mpop(2)
        
        # Append else condition
        if expression and expression[0] == "t":
            # Pop off 't' and append else result
            expression.mpop()
            condNode.children.append(self.parse(expression))

        return condNode

    # Parse the function special form
    def parseFn(self, expression: ParseList) -> Node:
        # Pop next func name and opening paren: [funcName, (]
        fnName = Node(expression.mpop(), AtomType.SYMBOL) 
        expression.mpop()

        # Pop arguments 
        arguments = []
        while expression[0] != ")":
            arguments.append(expression.mpop())
        arguments = Node(arguments, AtomType.LIST)
        expression.mpop()

        # Pop doc string
        nextToken = expression[0]
        docstring = Node("", AtomType.STRING)
        if nextToken[0] == "\"" and nextToken[-1] == "\"":
            docstring = Node(nextToken, AtomType.STRING)
            expression.mpop()
        
        # Parse the rest of the function
        fndef = self.parse(expression)
        
        # Build function node
        newFn = Node("defun", BuiltIn.DEFUN, [fnName, arguments, docstring, fndef])
        
        return newFn    
    
    # Parse the quote special form 
    def parseQuote(self, expression: ParseList) -> Node:
        quote = Node("quote", BuiltIn.QUOTE, [self.parse(expression)])
        
        return quote
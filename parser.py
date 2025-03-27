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
        
        self.SPECIAL_FORMS: Dict[str, tuple[Callable[[Parser.ParseList, str, BuiltIn], Node], str, BuiltIn]] = {
            "defun" : [self.parseFn, "defun", BuiltIn.DEFUN],
            "defmacro" : [self.parseFn, "defmacro", BuiltIn.DEFMACRO],
            "lambda" : [self.parseLambda, "lambda", BuiltIn.LAMBDA],
            
            "cond" : [self.parseCond, "cond", BuiltIn.COND],
            # "do" : [self.parseDo, "do", BuiltIn.DO],
            
            "quote" : [self.parseQuote, "quote", BuiltIn.QUOTE],
            "'" : [self.parseQuote, "quote", BuiltIn.QUOTE],
            "`" : [self.parseQuasiQuote, "quasi-quote", BuiltIn.QUASIQUOTE],
            "," : [self.parseQuote, "unquote", BuiltIn.UNQUOTE]
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
    # Set quote to true to return a list
    def parse(self, expression: ParseList, asList = False) -> Node:
        nextToken = expression.mpop()
        
        # New expression
        if nextToken == "(":
            # Get the head value
            head = expression.mpop()
            head = self.tokenToNode(head)
            
            # Handle special forms
            if head.val in self.SPECIAL_FORMS:
                return self.handleSpecialForm(expression, head.val)

            # Process as list
            if (head.type != AtomType.SYMBOL and type(head.type) != BuiltIn) or asList:
                head = Node(None, AtomType.LIST, [head]) # Old head becomes first element
                
            # Process arguments 
            while expression[0] != ")":
                head.children.append(self.parse(expression))
            expression.mpop() # Pop closing parentheses
            
            return head
        # Inside expression
        else:
            # Check for special form
            if nextToken in self.SPECIAL_FORMS:
                return self.handleSpecialForm(expression, nextToken)
            # Convert to node
            return self.tokenToNode(nextToken)
    
    # Call the appropriate special form
    def handleSpecialForm(self, expression: ParseList, type: str) -> Node:
        specialForm = self.SPECIAL_FORMS[type]
        return specialForm[0](expression, specialForm[1], specialForm[2])
        
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
    def parseCond(self, expression: ParseList, name: str, type: BuiltIn) -> Node:
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

    # Parse the defun special form
    def parseFn(self, expression: ParseList, name: str, type: BuiltIn) -> Node:
        # Pop next func name and opening paren: [funcName, (]
        fnName = Node(expression.mpop(), AtomType.SYMBOL) 
        expression.mpop()

        # Pop arguments 
        arguments = Node(None, AtomType.LIST)
        while expression[0] != ")":
            arguments.children.append(self.parse(expression, asList=True))
        expression.mpop()

        # Pop doc string
        nextToken = expression[0]
        docstring = Node("", AtomType.STRING)
        if nextToken[0] == "\"" and nextToken[-1] == "\"":
            docstring = Node(nextToken, AtomType.STRING)
            expression.mpop()
        
        # Parse the body of the function as a list of expression
        body = Node(None, AtomType.LIST)
        while expression[0] != ")":
            body.children.append(self.parse(expression))
        expression.mpop()
        
        # Build function node
        newFn = Node(name, type, [fnName, docstring, arguments, body])
        
        return newFn    

    def parseLambda(self, expression: ParseList, name: str, type: BuiltIn) -> Node:
        # Parse the arguments 
        arguments = self.parse(expression, asList=True)

        # Parse the body of the function as a list of expressions
        body = Node(None, AtomType.LIST)
        while expression[0] != ")":
            body.children.append(self.parse(expression))
        expression.mpop()
        
        lambdaFn  = Node(name, type, [arguments, body])
        
        return lambdaFn
    
    # Parse quote and unquote:
    # (quote (var1 var2 ...)) OR '(var1 var2) OR 'var1
    # `(+ ,x ,y)
    def parseQuote(self, expression: ParseList, name: str, type: BuiltIn) -> Node:
        quote = Node(name, type, [self.parse(expression, asList=True)])
        
        return quote

    # Parse quasi-quote  
    def parseQuasiQuote(self, expression: ParseList, name: str, type: BuiltIn) -> Node:
        quasiQuote = Node("quasi-quote", BuiltIn.QUASIQUOTE)
        
        while expression[0] != ")":
            quasiQuote.children.append(self.parse(expression))

        return quasiQuote
    
    # Parse the do special form:
    # (do ((var1 init1 step1) 
    #      (var2 init2 step2) 
    #      (...)
    #      (end-condition *result)
    #  (body)
    #  (body2)
    #  (...))
    # *returning a "result" is optional 
    def parseDo(self, expression: ParseList, name: str) -> Node:
        print(expression)
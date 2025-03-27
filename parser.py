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
        
        self.SPECIAL_FORMS: Dict[str, Callable[[Parser.ParseList, str], Node]] = {
            "defun" : [self.parseFn, "defun"],
            "defmacro" : [self.parseFn, "defmacro"],
            "lambda" : [self.parseLambda, "lambda"],
            
            "cond" : [self.parseCond, "cond"],
            "do" : [self.parseDo, "do"],
            
            "quote" : [self.parseQuote, "quote"],
            "'" : [self.parseQuote, "quote"],
            "`" : [self.parseQuasiQuote, "quasi-quote"],
            "," : [self.parseQuote, "unquote"]
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

            # Process as list
            if (head.type != AtomType.SYMBOL and type(head.type) != BuiltIn) or asList:
                head = Node(None, AtomType.LIST, [head]) # Old head becomes first element
                
            # Handle special forms
            if head.val in self.SPECIAL_FORMS:
                return self.handleSpecialForm(expression, head.val)
            
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
        return specialForm[0](expression, specialForm[1])
        
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
    def parseCond(self, expression: ParseList, name: str) -> Node:
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
    def parseFn(self, expression: ParseList, name: str) -> Node:
        # Pop next func name and opening paren: [funcName, (]
        fnName = Node(expression.mpop(), AtomType.SYMBOL) 
        expression.mpop()

        # Pop arguments 
        arguments = Node(None, AtomType.LIST)
        while expression[0] != ")":
            arguments.children.append(self.parse(expression))
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
        newFn = Node(name, BuiltIn.DEFUN, [fnName, arguments, docstring, fndef])
        
        return newFn    

    def parseLambda(self, expression: ParseList, name: str) -> Node:
        print("Parsing Lambda")
        print(expression)
        arguments = self.parse(expression)
        # arguments.append(self.parse(expression))
        print(arguments)
        arguments = Node("arguments", AtomType.LIST, [arguments])
        print(arguments)
        lambdaDef = self.parse(expression)
        
        lambdaFn  = Node(name, BuiltIn.LAMBDA, [arguments, lambdaDef])
        return lambdaFn
    
    # Parse quote and unquote:
    # (quote (var1 var2 ...)) OR '(var1 var2) OR 'var1
    # `(+ ,x ,y)
    def parseQuote(self, expression: ParseList, name: str) -> Node:
        quote = Node(name, BuiltIn.QUOTE, [self.parse(expression, asList=True)])
        
        return quote

    # Parse quasi-quote  
    def parseQuasiQuote(self, expression: ParseList, name: str) -> Node:
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
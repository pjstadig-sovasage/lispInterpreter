import re
from typing import List

class Lexer:
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
        print(filename)
        with open(filename, "r", encoding="utf-8") as file:
            for line in file:
                for match in self.TOKEN_REGEX.finditer(line):
                    token = match.group().strip()
                    if not token or token.startswith(";"):  # Ignore whitespace and comments
                        continue
                    yield token
    
    # Split a tokenized file into its individual expressions
    def chunkFile(self, tokenizedFile: List[str]) -> List[List[str]]:
        paren = 0
        tokenizedExpressions = []
        currentExpression = []
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
                    currentExpression = []

        return tokenizedExpressions
        
    # Turn string expression into a list of tokens
    def tokenizeString(self, expression: str):
        expression = expression.replace("'", " (quote ")
        expression = expression.replace("(", " ( ").replace(")", " ) ")

        tokenizedExpression = []
        for match in self.TOKEN_REGEX.finditer(expression):
            token = match.group().strip()
            if not token:
                continue
            tokenizedExpression.append(token)
            
        return tokenizedExpression
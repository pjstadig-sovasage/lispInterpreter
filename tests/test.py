import sys
from lexer import Lexer
from parser import Parser
from node import Node

def runREPLTest():
    lexer, parser = Lexer(), Parser()
    
    for string in testStrings:
        tokenizedString = lexer.tokenizeString(string)
        print(tokenizedString)
        ast = parser.parseExpression(tokenizedString)
        ast.print_tree()

import sys
from lexer import Lexer
from parser import Parser
from node import Node

testStrings = [
    "(defun factorial (n) \"Computes the factorial of a number recursively.\" (if (<= n 1) 1 (* n (factorial (- n 1)))))",
    "(defun fibonacci (n) \"Computes the nth Fibonacci number using recursion.\" (if (<= n 1) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))",
    "(cond ((> x 10) \"Greater than 10\") ((< x 10) \"Less than 10\") ((= x 10) \"Equal to 10!\") (t 'other))",
    "(defun filter (pred lst) \"Filters a list based on a predicate function.\" (cond ((null lst) '()) ((funcall pred (car lst)) (cons (car lst) (filter pred (cdr lst)))) (t (filter pred (cdr lst)))))"
]

def runFastTest():
    lexer, parser = Lexer(), Parser()
    
    for string in testStrings:
        tokenizedString = lexer.tokenizeString(string)
        ast = parser.parseExpression(tokenizedString)
        ast.print_tree()

def runFileParse(filename: str):
    lexer, parser = Lexer(), Parser()
    tokenizedFile = lexer.tokenizeFile(filename)
    tokenizedExpressions = lexer.chunkFile(tokenizedFile)
    print(tokenizedExpressions)
    parsedExpressions = parser.parseExpressionList(tokenizedExpressions)
    
    for expression in parsedExpressions:
        expression.print_tree()

if __name__ == "__main__":
    if "--file" in sys.argv:
        print("Parsing file...")
        fileIndex = sys.argv.index('--file')
        if fileIndex + 1 < len(sys.argv):
            filename = sys.argv[fileIndex + 1]
            runFileParse(filename)
    else:
        print("No file, running fast test...")
        runFastTest()
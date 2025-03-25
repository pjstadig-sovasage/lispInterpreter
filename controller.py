from lexer import Lexer
from parser import Parser

class Controller:
    def __init__(self):
        self.lexer = Lexer()
        self.parser = Parser()
    
    # Evaluate command line input line by line
    def runREPL():
        lexer, parser = Lexer(), Parser()
        running = True
        
        print("Enter LISP expressions one-by-one for evaluation. Type q to exit.")
        while running:
            try: 
                user_input = input(">>> ")
                if user_input == "q":
                    print("Exiting LISP Interpreter")
                    running = False
                else:
                    tokenizedString = list(lexer.tokenizeString(user_input))
                    print(f"Tokenized expression: {tokenizedString}")
                    ast = parser.parseExpression(tokenizedString)
                    print("AST:"), ast.print_tree()
            except Exception as e:
                print(f"Error encountered: {e}")

    # Evaluate full file
    def runFileParse(filename: str):
        lexer, parser = Lexer(), Parser()
        tokenizedFile = lexer.tokenizeFile(filename)
        tokenizedExpressions = lexer.chunkFile(tokenizedFile)
        print(tokenizedExpressions)
        parsedExpressions = parser.parseExpressionList(tokenizedExpressions)
        
        for expression in parsedExpressions:
            expression.print_tree()
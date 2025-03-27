from lexer import Lexer
from parser import Parser

class Controller:
    def __init__(self):
        self.lexer = Lexer()
        self.parser = Parser()

    # Evaluate command line input line by line
    def runREPL(self):
        running = True

        print("Enter LISP expressions one-by-one for evaluation. Type q to exit.")
        while running:
            try:
                user_input = input(">>> ")
                if user_input == "q":
                    print("Exiting LISP Interpreter")
                    running = False
                else:
                    tokenizedString = list(self.lexer.tokenizeString(user_input))
                    print(f"Tokenized expression: {tokenizedString}")
                    ast = self.parser.parseExpression(tokenizedString)
                    print("AST:"), ast.print_tree()
            except Exception as e:
                print(f"Error encountered: {e}")

    # Evaluate full file and output to log
    def runFile(self, filename: str, printConsole=True):
        tokenizedFile = list(self.lexer.tokenizeFile(filename))
        tokenizedExpressions = self.lexer.chunkFile(tokenizedFile)
        parsedExpressions = self.parser.parseExpressionList(tokenizedExpressions)

        # Print to console
        if printConsole:
            for i in range(0, len(tokenizedExpressions)):
                print(tokenizedExpressions[i])
                print(parsedExpressions[i])

        # Output log
        with open(filename.replace("tests", "results"), 'w') as outFile:
            for i in range(0, len(tokenizedExpressions)):
                outFile.write(str(tokenizedExpressions[i]) + '\n')
                outFile.write(parsedExpressions[i].astString() + '\n')
